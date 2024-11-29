case class CnnHwAcceleratorParams (
    address: BigInt = 0x40000,
    vectorSize: Int = 8,
    maxSize: Int = 4096)

case object CnnHwAcceleratorKey extends Field[Option[CnnHwAcceleratorParams]](None)

class CnnHwAcceleratorIO(
    val BUS_ADDR_WIDTH: Int,
    val BUS_DATA_WIDTH: Int,
    val VECTOR_SIZE: Int,
    val MAX_SIZE: Int) extends Bundle {

    val DIM_WIDTH = log2Ceil(MAX_SIZE) + 1
    val NUM_BYTES = BUS_DATA_WIDTH/8

    val clkIn      = Input(Clock())
    val rstIn      = Input(Bool())
    val startIn    = Input(Bool())
    val filtRowsIn = Input(UInt(DIM_WIDTH.W))
    val filtColsIn = Input(UInt(DIM_WIDTH.W))
    val dataRowsIn = Input(UInt(DIM_WIDTH.W))
    val dataColsIn = Input(UInt(DIM_WIDTH.W))
    val addrIn     = Input(UInt(BUS_ADDR_WIDTH.W))
    val wrEnIn     = Input(UInt(NUM_BYTES.W))
    val wrDataIn   = Input(UInt(BUS_DATA_WIDTH.W))
    val readyIn    = Input(Bool())
    val validOut   = Output(Bool())
    val dataOut    = Output(UInt(32.W))
}

class CnnHwAcceleratorBlackBox(
    val BUS_ADDR_WIDTH: Int,
    val BUS_DATA_WIDTH: Int,
    val VECTOR_SIZE: Int,
    val MAX_SIZE: Int) extends BlackBox(Map(
    "BUS_ADDR_WIDTH" -> IntParam(BUS_ADDR_WIDTH),
    "BUS_DATA_WIDTH" -> IntParam(BUS_DATA_WIDTH),
    "VECTOR_SIZE"    -> IntParam(VECTOR_SIZE),
    "MAX_SIZE"       -> IntParam(MAX_SIZE))) with HasBlackBoxResource {

    val io = IO(new CnnHwAcceleratorIO(
        BUS_ADDR_WIDTH,
        BUS_DATA_WIDTH,
        VECTOR_SIZE,
        MAX_SIZE))
    
    addResource("/vsrc/cnn_hw_accelerator.v")
}

class CnnHwAcceleratorManager(params: CnnHwAcceleratorParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
    val device = new SimpleDevice("cnn-hw-accelerator-manager", Seq("cpu"))
    val node = TLRegisterNode(
        address = Seq(AddressSet(params.address, 4096-1)), 
        device = device,
        beatBytes = beatBytes)

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this)
    {
        val io = IO(new Bundle {
            val dataAddr   = Output(UInt(64.W))
            val dataCols   = Output(UInt(32.W))
            val dataRows   = Output(UInt(32.W))
            val filtAddr   = Output(UInt(64.W))
            val filtCols   = Output(UInt(32.W))
            val filtRows   = Output(UInt(32.W))
            val destAddr   = Output(UInt(64.W))
            val start      = Output(Bool())
        })

        val dataAddr   = RegInit(0.U(64.W))
        val dataCols   = RegInit(0.U(32.W))
        val dataRows   = RegInit(0.U(32.W))
        val filtAddr   = RegInit(0.U(64.W))
        val filtCols   = RegInit(0.U(32.W))
        val filtRows   = RegInit(0.U(32.W))
        val destAddr   = RegInit(0.U(64.W))
        val start      = Wire(new DecoupledIO(Bool()))
        start.ready   := true.B

        node.regmap(
            0x00 -> Seq(RegField(64, dataAddr)),
            0x08 -> Seq(RegField(32, dataCols)),
            0x0C -> Seq(RegField(32, dataRows)),
            0x10 -> Seq(RegField(64, filtAddr)),
            0x18 -> Seq(RegField(32, filtCols)),
            0x1C -> Seq(RegField(32, filtRows)),
            0x20 -> Seq(RegField(64, destAddr)),
            0x28 -> Seq(RegField.w(1, start)))

        io.dataAddr   := dataAddr
        io.dataCols   := dataCols
        io.dataRows   := dataRows
        io.filtAddr   := filtAddr
        io.filtCols   := filtCols
        io.filtRows   := filtRows
        io.destAddr   := destAddr
        io.start      := start.bits && start.valid
    }
}

class CnnHwAcceleratorClient(beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
    val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
        name = "cnn-hw-accelerator-client",
        sourceId = IdRange(0, 1))))))

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this)
    {
        
        val io = IO(new Bundle {
            val dataAddr   = Input(UInt(64.W))
            val dataCols   = Input(UInt(32.W))
            val dataRows   = Input(UInt(32.W))
            val filtAddr   = Input(UInt(64.W))
            val filtCols   = Input(UInt(32.W))
            val filtRows   = Input(UInt(32.W))
            val destAddr   = Input(UInt(64.W))
            val start      = Input(Bool())
        })

        def getMask(addr: UInt, size: UInt) : UInt =
        {
            val addrStart = addr(blockBytesLog2-1, 0)
            val sizeTrunc = size(blockBytesLog2-1, 0)
            val addrEnd = WireInit(0.U((blockBytesLog2+1).W))
            when (size < blockBytes.U)
            {
                addrEnd := addrStart +& size(blockBytesLog2-1, 0)
            }
            .otherwise
            {
                addrEnd := addrStart +& blockBytes.U
            }
            val mask = VecInit(Seq.fill(blockBytes)(false.B))
            for (i <- 0 to (blockBytes-1))
            {
                when ((addrStart <= i.U) && (i.U < addrEnd))
                {
                    mask(i) := true.B
                }
            }
            return mask.asUInt
        }

        val (tl, edge) = node.out(0)

        val busAddrWidth = edge.bundle.addressBits
        val busDataWidth = 8 * beatBytes

        val accelerator = CnnHwAcceleratorBlackBox(
            busAddrWidth,
            busDataWidth,
            p.vectorSize,
            p.maxSize)

        val readFifo    = Module(new Fifo(UInt((addrBits + blockBytes).W), 8, 1))
        val writeFifo   = Module(new Fifo(UInt((dataBits + addrBits + blockBytes + 1).W), 8, 1))
        
        switch (readSelStateR) {
            is (ReadSelState.ReadData) {
                readAddrR           := io.dataAddr(addrBits-1, 0)
                filtAddrR           := io.filtAddr(addrBits-1, 0)
                readSizeR           := Cat(io.dataCols * io.dataRows, 0.U(dataBytesLog2))
                filtSizeR           := Cat(io.filtCols * io.filtRows, 0.U(dataBytesLog2))
                if (io.start) {
                    readStartR      := true.B
                    readSelStateR   := State.ReadFilt
                }
            }
            is (ReadSelState.ReadFilt) {
                readAddrR           := filtAddrR
                readSizeR           := filtSizeR    
                when (readDone2R) {
                    readStartR      := false.B
                    readSelStateR   := State.ReadData
                }
            }
        }

        readValid2R                 := false.B
        readDone2R                  := false.B
        switch (readState2R) {
            is (ReadState.Idle) {
                readSize2R          := readSizeR        
                readAddrNext2R      := readAddrR
                numBytesRead2R      := blockBytes.U - readSizeR(blockBytesLog2 - 1, 0)
                when (readStartR) {
                    readState2R     := ReadState.Read
                }
            }
            is (ReadState.Read) {
                when (readFifo.io.enq.ready) {
                    readValid2R     := true.B
                    numBytesRead2R  := blockBytes.U
                    readSize2R      := readSize2R - numBytesRead2R
                    readAddrNext2R  := readAddrNext2R + numBytesRead2R
                    when (readSize2R <= numBytesRead2R) {
                        readState2R := ReadState.Idle
                        readDone2R  := true.B
                    }
                }
            }
        }

        readAddr2R  := readAddrNext2R

        readValid3R := readValid2R
        readAddr3R  := Cat(readAddr2R(addrBits-1, blockBytesLog2), 0.U(blockBytesLog2))
        readMask3R  := getMask(readAddr2R, readSize2R)


        switch (readStateR)
        {
            is (ReadState.Idle) {
                numBytesReadR       := blockBytes.U - readAddrR(blockBytesLog2 - 1, 0)
                bytesLeftReadR      := readSizeR
                when (readStartR) {
                    readStateR      := ReadState.Read
                }
            }
            is (ReadState.Read) {
                when (readFifo.io.enq.ready) {
                    numBytesReadR   := blockBytes.u
                    
                }
            }

            when (readStartR){
                readStateR  := 
            }
        }

        if (postIdR == claimIdR)
        {

        }

        when (tl.a.fire && !tl.d.fire) {
            cntR        := cntR + 1
        } .elsewhen (!t1.a.fire && tl.d.fire) {
            cntR        := cntR - 1;
        }

        when (tl.a.fire) {
            postIdR     := postIdR + 1
        }

        when (tl.d.fire) {
            claimIdR    := claimIdR + 1
        }


    }
}

class ReadQueue()
