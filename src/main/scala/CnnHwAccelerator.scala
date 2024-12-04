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

        val readCtrl    = Module(new ReadController (maxSize, elemWidth, busAddrWidth, beatBytes))
        val readAlign   = Module(new AlignmentBuffer(maxSize, elemWidth, addrWidth,    beatBytes))
        val writeAlign  = Module(new AlignmentBuffer(maxSize, elemWidth, busAddrWidth, beatBytes))

        val readFifo    = Module(new Fifo(UInt((addrBits + blockBytes).W), 8, 2))
        val writeFifo   = Module(new Fifo(UInt((dataBits + addrBits + blockBytes + 1).W), 8, 2))

        readStartR      := false.B
        switch (readStateR) {
            is (ReadState.Idle) {
                when (io.startIn) {
                    readAddrR(0)        := io.dataAddr
                    readAddrR(1)        := io.filtAddr
                    readSizeR(0)        := Cat(io.dataCols * io.dataRows, 0.U(dataBytesLog2))
                    readSizeR(1)        := Cat(io.filtCols * io.filtRows, 0.U(dataBytesLog2))
                    readValidR          := 3.U
                    readStartR          := true.B
                }
            }
            is (ReadState.Read) {
                when (readCtrl.io.lastOut) {
                    readValidR          := readValidR >> 1
                    readAddrR(0)        := readAddrR(1)
                    readSizeR(0)        := readSizeR(1)
                    when (readValidR === 1.U) {
                        readStateR      := ReadState.Idle
                    } .otherwise {
                        readStartR      := true.B
                    }
                }
            }
        }

        /* Connect FSM to Read Controller */
        readCtrl.io.startIn         := readStartR
        readCtrl.io.sizeIn          := readSizeR(0)
        readCtrl.io.sourceAddrIn    := readAddrR(0)

        /* Connect Read Controller to Read FIFO */
        readCtrl.io.rdReadyIn       := readFifo.io.enq.ready
        readFifo.io.enq.valid       := readCtrl.io.rdValidOut
        readFifo.io.enq.bits        := Cat(readCtrl.io.rdEnOut, readCtrl.io.addrOut)

        /* Connect Read FIFO to arbitrator */
        readFifo.io.deq.ready       := readyReadyR
        
        
        tl_d_dataR                  := edge.data(tl.d.bits)

        /* Connect Arbitrator to Read Alignment Buffer*/
        rdAlignAddrMsbR              = RegInit(false.B)
        when (cfgStartR(0)) {
            rdAlignAddrMsbR         := not rdAlignAddrMsbR
        }

        
        tl_d_readyR                 := readAlign.io.wrReadyOut
        readAlign.io.wrValidIn      := tl_d_validR
        readAlign.io.wrDataIn       := tl_d_dataR
        readAlign.io.wrReadyIn      := true.B

        /* Read Alignment Buffer Configuration Pipeline */
        readAlignStartR             : false.B
        when (io.startIn) {
            readAlignStartR         := true.B
            readAlignValidR         := 3.U
            readAlignSourceR(0)     := io.dataAddrIn
            readAlignSourceR(1)     := io.filtAddrIn
            readAlignSizeR(0)       := io.dataColsIn * io.dataRowsIn
            readAlignSizeR(1)       := io.filtColsIn * io.filtRowsIn
        } .elsewhen (readAlign.io.lastOut) {
            readAlignValidR         := readAlignValidR >> 1.U
            readAlignSourcR(0)      := readAlignSourceR(1)
            readAlignSizeR(0)       := readAlignSizeR(1)
            when (readAlignValidR(1))  
                readAlignStartR     := true.B
            }
        }

        /* Read Alignment Buffer Configuration */
        readAlign.io.startIn        := readAlignStartR
        readAlign.io.sizeIn         := readAlignSizeR(0)
        readAlign.io.destAddrIn     := Cat(!readAlignValidR(1), 0.U(addrWidth-1))
        readAlign.io.sourcAddrIn    := readAlignSourceR(0)

        /* Connect Hardware Accelerator to Read Alignment Buffer */
        // Compute Hardware Accelerator Start Signal        
        accStartR                   := RegInit(false.B)
        lastStickyR                 := RegInit(false.B)

        accStartR                   := false.B
        when (readAlign.io.lastOut) {
            when (lastStickyR) {
                accStartR           := true.B
                lastStickyR         := false.B
            } .otherwise {
                lastStickyR         := true.B
            }
        }

        // Clock Hardware Accelerator Dimensions on Start
        // Should not see another start signal until computation is complete
        filtRowsR                    = Reg(UInt(countWidth.W))
        filtColsR                    = Reg(UInt(countWidth.W))
        when (io.startIn) {
            filtRowsR               := io.filtRowsIn
            filtColsR               := io.filtColsIn
            dataRowsR               := io.dataRowsIn
            dataColsR               := io.dataColsIn
        }

        // Source Connections
        accelerator.io.filtRowsIn   := filtRowsR
        accelerator.io.filtColsIn   := filtColsR
        accelerator.io.dataRowsIn   := dataRowsR
        accelerator.io.dataColsIn   := dataColsR
        
        accelerator.io.startIn      := accStartR
        accelerator.io.wrEnIn       := readAlign.io.wrEnOut
        accelerator.io.wrDataIn     := readAlign.io.wrDataOut
        accelerator.io.addrIn       := readAlign.io.addrOut
        
        val accWrValid               = WireInit(false.B)
        accWrValid                  := accelerator.io.validOut & wrAlign.io.rdReadyOut

        // Sink Connections
        when (io.startIn) {
            writeShiftR             := 0.U
        } .elseWhen (accWrValid) {
            writeShiftR             := writeShiftR + 1.U
        }

        val wrEnHi                  := 15.U(4.W)
        val wrEnLo                  := 0.U(4.W)
        
        val accWrEn                  = WireInit(UInt(4.W))
        val accWrData                = WireInit(UInt(32.W))
        accWrEn                     := accWrValid ? wrEnHi : wrEnLo
        accWrData                   := accelerator.io.dataOut

        val accWrEnR                 = RegInit(0.U(beatBytes.W))
        val accWrDataR               = RegInit(0.U(dataWidth.W))

        accWrEnR                    := Cat(0.U((beatBytes -  4).W),   accWrEn) << Cat(writeShiftR, 0.U(2.W))
        accWrDataR                  := Cat(0.U((dataWidth - 32).W), accWrData) << Cat(writeShiftR, 0.U(5.W))

        when (io.startIn) {
            outSizeR                := 
        }

        wrAlign.io.startIn          := cfgStartR(1)
        wrAlign.io.sizeIn           := outSizeR
        wrAlign.io.destAddrIn       := destAddrR
        wrAlign.io.sourceAddrIn     := 0.U
        wrAlign.io.wrValidIn        := accWrEnR
        wrAlign.io.wrDataIn         := accWrDataR

        wrAlign.io.wrReadyIn        := writeFifo.io.enq.ready
        writeFifo.io.enq.valid      := wrAlign.io.wrEnOut =/= 0.U
        writeFifo.io.enq.data       := Cat(writeAlign.io.lastOut, 
                                           writeAlign.io.addrOut,
                                           writeAlign.io.wrEnOut,
                                           writeAlign.io.wrDataOut)

        val wrFifoDataLo             = 0
        val wrFifoDataHi             = wrFifoDataLo + dataWidth - 1
        val wrFifoEnLo               = wrFifoDataHi + 1
        val wrFifoEnHi               = wrFifoEnLo + beatBytes - 1
        val wrFifoAddrLo             = wrFifoEnHi + 1
        val wrFifoAddrHi             = wrFifoAddrLo + addrWidth - 1
        val wrFifoLastIdx            = wrFifoAddrHi + 1

        val doneR                    = RegInit(false.B)
        val arbStateR                = RegInit(ArbState.Idle)
        val tl_d_lastR               = Reg(Bool())
        val tl_a_bitsR               = Reg(tl.a.bits.cloneType)
        val tl_a_validR              = RegInit(false.B)
        val wrFifoReadyR             = RegInit(false.B)

        val tl_a_data                = WireInit(0.U(busDataWidth.W))
        val tl_a_addr                = WireInit(0.U(busAddrWidth.W))
        val tl_a_mask                = WireInit(0.U(beatBytes.W))
                     

        doneR                       := false.B
        switch (arbStateR) {
            is (ArbState.Idle) {
                when (writeFifo.io.deq.valid) {
                    tl_d_lastR      := writeFifo.io.deq.bits(wrFifoLastIdx)
                    tl_a_data       := writeFifo.io.deq.bits(wrFifoDataHi, wrFifoDataLo)
                    tl_a_addr       := writeFifo.io.deq.bits(wrFifoAddrHi, wrFifoAddrLo)
                    tl_a_mask       := writeFifo.io.deq.bits(wrFifoEnHi, wrFifoEnLo)
                    tl_a_bitsR      := edge.Put(0.U, tl_a_addr, log2Ceil(blockBytes).U, tl_a_data, tl_a_mask)._2
                    tl_a_validR     := true.B
                    wrFifoReadyR    := true.B
                    arbStateR       := ArbState.WaitA
                } .elsewhen (readFifo.io.deq.ready) {
                    tl_a_addr       := readFifo.io.deq.bits(addrBits+blockBytes-1, blockBytes)
                    tl_a_bitsR      := edge.Get(0.U, tl_a_addr, log2Ceil(blockBytes).U)._2
                    tl_a_validR     := true.B
                    readReadyR      := true.B
                    arbStateR       := ArbState.WaitA
                }
            }
            is (ArbState.WaitA) {
                when (tl.a.fire) {
                    tl_a_validR     := false.B
                    tl_d_readyR     := readAlign.io.wrReadyOut
                    arbStateR       := ArbState.WaitD
                }
            }
            is (ArbState.WaitD) {
                tl_d_readyR         := readAlign.io.wrReadyOut
                when (tl.d.fire) {
                    tl_d_readyR     := false.B
                    tl_d_validR     := edge.hasData(tl.d.bits)
                    when (tl_d_lastR) {
                        doneR       := true.B
                    }
                }
            }
        }

        tl_d_bitsR                  := 
    }
}

class ReadController(maxSize: Int, elemWidth: Int, addrWidth: Int, beatBytes: Int) extends Module
{
    val sizeWidth  = log2Ceil(maxSize)
    val elemBytes  = elemWidth / 8
    val countWidth = sizeWidth + log2Ceil(elemBytes)
    val dataWidth  = 8 * beatBytes
    val shiftWidth = log2(ceil(beatBytes))

    val io = IO(new Bundle{
        val startIn      = Input(Bool())
        val sizeIn       = Input(UInt(sizeWidth.W))
        val sourceAddrIn = Input(UInt(addrWidth.W))
        val lastOut      = Output(Bool())
        val rdReadyIn    = Input(Bool())
        val rdValidOut   = Output(Bool())
        val rdEnOut      = Output(UInt(beatBytes.W))
        val addrOut      = Output(UInt(addrWidth.W))
    })

    def getMask(bytesLeft : UInt) : UInt {
        val mask = WireInit(0.U(beatBytes.W))
        when (bytesLeft >= blockBytes.U) {
            mask  := -1.S(beatBytes.W).asUInt
        } .otherwise {
            for (i <- 0 to (blockBytes-1)) {
                when (bytesLeft(log2Ceil(beatBytes)-1, 0) > i.U)
                {
                    mask(i) := true.B
                }
            }
        }
        return mask
    }

    object State extends ChiselEnum
    {
        val Init, Read = Value
    }

    val sizeBytes    = WireInit(0.U(countWidth.W))
    sizeBytes       := Cat(io.sizeIn, 0.U(log2Ceil(elemBytes).W))

    validR           = RegInit(false.B)
    stateR           = RegInit(State.Init)
    shiftR           = Reg(UInt(sourceWidth.W))
    bytesLeftR       = Reg(UInt(countWidth.W))
    bytesReadR       = Reg(UInt((shiftWidth+1).W))
    nextAddrR        = Reg(UInt(addrWidth.W))

    validR          := false.B
    switch (stateR) {
        is (State.Init) {
            when (io.startIn) {
                shiftR          := io.sourceAddrIn(shiftWidth-1, 0)
                bytesReadR      := beatBytes.U - io.sourceAddrIn(shiftWidth-1, 0)
                bytesLeftR      := sizeBytes
                nextAddrR       := io.sourceAddrIn
                stateR          := State.Read
            }
        }
        is (State.Read) {
            when (io.rdReadyIn) {
                validR          := true.B
                bytesReadR      := beatBytes.U
                bytesLeftR      := bytesLeftR - bytesReadR
                nextAddrR       := nextAddrR + bytesReadR
                when (bytesLeftR <= bytesReadR) {
                    stateR      := State.Init
                }
            }
        }
    }

    addrR    = Reg(UInt(addrWidth.W))
    rdEnR    = Reg(UInt(beatBytes.W))
    lastR    = Reg(Bool())

    addrR   := nextAddr
    rdEnR   := getMask(bytesLeftR)
    lastR   := bytesLeft <= bytesReadR ? true.B : false.B

    valid2R  = RegInit(false.B)
    addr2R   = Reg(UInt(addrWidth.W))
    rdEn2R   = Reg(UInt(beatBytes.W))
    last2R   = Reg(Bool())

    valid2R := validR
    addr2R  := Cat(addrR(addrWidth, shiftWidth), 0.U(shiftWidth.W))
    rdEn2R  := rdEnR << shiftR
    last2R  := lastR

    io.rdValidOut   := valid2R
    io.rdEnOut      := rdEn2R
    io.addrOut      := addr2R
    io.lastOut      := last2R
}

class AlignmentBuffer(maxSize: Int, elemWidth: Int, addrWidth: Int, beatBytes: Int) extends Module
{

    val sizeWidth  = log2Ceil(maxSize)
    val elemBytes  = elemWidth / 8
    val countWidth = sizeWidth + log2Ceil(elemBytes)
    val dataWidth  = 8 * beatBytes
    val shiftWidth = log2(ceil(beatBytes))

    val io = IO(new Bundle{
        val startIn      = Input(Bool())
        val sizeIn       = Input(UInt(sizeWidth.W))
        val destAddrIn   = Input(UInt(addrWidth.W))
        val sourceAddrIn = Input(UInt(addrWidth.W))
        val wrValidIn    = Input(UInt(beatBytes.W))
        val wrDataIn     = Input(UInt(dataWidth.W))
        val wrReadyOut   = Output(Bool())        
        val wrReadyIn    = Input(Bool())
        val wrEnOut      = Output(UInt(beatBytes.W))
        val wrDataOut    = Output(UInt(beatBytes.W))
        val addrOut      = Output(UInt(addrWidth.W))
        val lastOut      = Output(Bool())
    })

    def getMask(bytesLeft : UInt) : UInt {
        val mask = WireInit(0.U(beatBytes.W))
        when (bytesLeft >= blockBytes.U) {
            mask  := -1.S(beatBytes.W).asUInt
        } .otherwise {
            for (i <- 0 to (blockBytes-1)) {
                when (bytesLeft(log2Ceil(beatBytes)-1, 0) > i.U)
                {
                    mask(i) := true.B
                }
            }
        }
        return mask
    }

    object State extends ChiselEnum
    {
        val Init, Read = Value
    }

    val sizeBytes    = WireInit(0.U(countWidth.W))
    sizeBytes       := Cat(io.sizeIn, 0.U(log2Ceil(elemBytes).W))

    val ctrlFifo     = Module(new Fifo(UInt((beatBytes+addrWidth+shiftWidth+1).W), 8, 3))

    validR           = RegInit(false.B)
    stateR           = RegInit(State.Init)
    shiftR           = Reg(UInt(sourceWidth.W))
    bytesLeftR       = Reg(UInt(countWidth.W))
    bytesReadR       = Reg(UInt((shiftWidth+1).W))
    nextAddrR        = Reg(UInt(addrWidth.W))

    validR          := false.B
    switch (stateR) {
        is (State.Init) {
            when (io.startIn) {
                shiftR          := io.destAddrIn(shiftWidth-1, 0) - io.sourceAddrIn(shiftWidth-1, 0)
                bytesReadR      := beatBytes.U - io.destAddrIn(shiftWidth-1, 0)
                bytesLeftR      := sizeBytes
                nextAddrR       := io.destAddrIn
                stateR          := State.Read
            }
        }
        is (State.Read) {
            when (ctrlFifo.io.enq.ready) {
                validR          := true.B
                bytesReadR      := beatBytes.U
                bytesLeftR      := bytesLeftR - bytesReadR
                nextAddrR       := nextAddrR + bytesReadR
                when (bytesLeftR <= bytesReadR) {
                    stateR      := State.Init
                }
            }
        }
    }

    addrR    = Reg(UInt(addrWidth.W))
    maskR    = Reg(UInt(beatBytes.W))
    lastR    = Reg(Bool())

    addrR   := nextAddr
    maskR   := getMask(bytesLeftR)
    lastR   := bytesLeft <= bytesReadR ? true.B : false.B

    valid2R  = RegInit(false.B)
    addr2R   = Reg(UInt(addrWidth.W))
    mask2R   = Reg(UInt(beatBytes.W))
    shift2R  = Reg(UInt())
    last2R   = Reg(Bool())

    valid2R := validR
    addr2R  := Cat(addrR(addrWidth, shiftWidth), 0.U(shiftWidth.W))
    mask2R  := (maskR << addrR(shiftWidth-1, 0)) >> addrR(shiftWidth-1, 0)
    shift2R := shiftR
    last2R  := lastR

    valid3R  = RegInit(false.B)
    addr3R   = Reg(UInt(addrWidth.W))
    mask3R   = Reg(UInt(beatBytes.W))
    shift3R  = Reg(UInt())
    last3R   = Reg(Bool())
    
    valid3R := valid2R
    addr3R  := addr2R
    mask3R  := (Cat(mask2R, mask2R) >> shift2R)(beatBytes-1, 0)
    shift3R := shift2R
    last3R  := last2R 

    ctrlFifo.io.enq.bits    := Cat(last3R, shift3R, mask3R, addr3R)
    ctrlFifo.io.enq.valid   := valid3R

    val addrLo      = 0
    val addrHi      = addrLo + addrWidth - 1
    val maskLo      = addrHi + 1
    val maskHi      = maskLo + beatBytes - 1
    val shiftLo     = maskHi + 1
    val shiftHi     = shiftLo + shiftWidth - 1
    val lastIdx     = shiftHi + 1
    
    byteFifoWrReady = WireInit(0.U(beatBytes.W))
    byteFifoRdValid = WireInit(0.U())
    byteFifoRdData  = WireInit(Seq.fill(blockBytes)(0.U(8.W)))

    for (i <- 0 to (blockBytes-1)) {
        val byteFifo             = Module(new Fifo(UInt(8.W), 8, 2))
        byteFifo.io.enq.valid   := io.wrValidIn(i)
        byteFifo.io.enq.bits    := (io.wrDataIn >> (8 * i))(7, 0)
        byteFifoWrReady(i)      := byteFifo.io.enq.valid
        byteFifo.io.deq.ready   := byteFifoRdReady(i) 
        byteFifoRdValid(i)      := byteFifo.io.deq.valid
        byteFifoRdData(i)       := byteFifo.io.deq.bits
    }
    
    wrReadyR         = RegInit(false.B)
    wrReadyR        := (byteFifoWrReady === -1.S(beatBytes).asUInt)

    io.wrReadyOut   := wrReadyR

    rdReady          = WireInit(false.B)
    rdReady         := wrReadyIn & ctrlFifo.io.deq.valid & (ctrlFifo.io.deq.bits === byteFifoRdValid)

    byteFifoRdReady  = WireInit(false.B)
    byteFifoRdReady := rdReady ? ctrlFifo.io.deq.bits(maskHi, maskLo) : 0.U

    wrEnOutR         = RegInit(0.U(beatBytes.W))
    wrDataOutR       = Reg(UInt(dataWidth.W))
    addrOutR         = Reg(UInt(addrWidth.W))
    shiftOutR        = Reg(UInt(shiftWidth.W))
    lastOutR         = RegInit(false.B)

    wrEnOutR        := byteFifoRdReady
    wrDataOutR      := byteFifoRdData.asUInt
    addrOutR        := ctrlFifo.io.deq.bits(addrHi, addrLo)
    shiftOutR       := ctrlFifo.io.deq.bits(shiftHi, shiftLo)
    lastOutR        := ctrlFifo.io.deq.bits(lastIdx)

    wrEnOut2R        = RegInit(0.U(beatBytes.W))
    wrDataOut2R      = Reg(UInt(dataWidth.W))
    addrOut2R        = Reg(UInt(addrWidth.W))
    lastOut2R        = RegInit(false.B)

    wrEnOut2R       := (Cat(wrEnOutR, wrEnOutR) >> shiftOutR)(beatBytes-1, 0)
    wrDataOut2R     := (Cat(wrDataOutR, wrDataOutR) >> Cat(shiftOutR, 0.U(3.W)))(dataWidth-1, 0)
    addrOut2R       := addrOutR
    lastOut2R       := lastOutR
    
    wrEnOut         := wrEnOut2R
    wrDataOut       := wrDataOut2R
    addrOut         := addrOut2R
    lastOut         := lastOut2R
}
