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
        
        doneR                       := false.B
        switch (arbStateR) {
            is (ArbState.Idle) {
                when (writeFifo.io.deq.valid) {
                    tl_d_lastR      := writeFifo.io.deq.bits(dataBits+addrBits+blockBytes)
                    tl_a_data       := writeFifo.io.deq.bits(dataBits+addrBits+blockBytes-1, addrBits+blockBytes)
                    tl_a_addr       := writeFifo.io.deq.bits(addrBits+blockBytes-1, blockBytes)
                    tl_a_mask       := writeFifo.io.deq.bits(blockBytes-1, 0)
                    tl_a_bitsR      := edge.Put(0.U, tl_a_addr, log2Ceil(blockBytes).U, tl_a_data, tl_a_mask)._2
                    tl_a_validR     := true.B
                    writeReadyR     := true.B
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
                tl_d_readyR         := readAlign.io.wrReadyOut & 
                when (tl.d.fire) {
                    tl_d_readyR     := false.B
                    tl_d_validR     := edge.hasData(tl.d.bits)
                    when (tl_d_lastR) {
                        doneR       := true.B
                    }
                }
            }
        }

        tl_d_dataR                  := edge.data(tl.d.bits)

        /* Connect Arbitrator to Read Alignment Buffer*/
        readAlign.io.startIn        := cfgStartR(0)
        readAlign.io.sizeIn         := cfgSize(0)
        readAlign.io.destAddrIn     := Cat(addrMsbR, 0.U(addrWidth-1))
        readAlign.io.sourcAddrIn    := cfgSource(0)
        readAlign.io.wrValidIn      := tl_d_validR
        readAlign.io.wrDataIn       := tl_d_dataR
        
        tl_d_readyR                 := readAlign.io.wrReadyOut

        addrMsbR                 = RegInit(false.B)
        addrMsbR                := cfgStartR(0) ? : not addrMsbR : addrMsbR



        for (i <- 0 to 1) {

            val cfgReadyR                = RegInit(false.B)
            val cfgStateR                = RegInit(ConfigState.Idle)

            val cfgFifo = Module(New Fifo(UInt((sizeWidth+2*addrWidth).W), 8, 0))
            cfgFifo.io.enq.valid        := readStartR
            cfgFifo.io.enq.bits         := Cat(readSizeR(0), readAddrR(0), destAddrR)
            cfgFifo.io.deq.ready        := cfgReadyR

            cfgSize(i)                  := cfgFifo.io.deq.bits(sizeHi, sizeLo)
            cfgSource(i)                := cfgFifo.io.deq.bits(sourceHi, sourceLo)
            cfgDest(i)                  := cfgFifo.io.deq.bits(destHi, destLo)

            cfgReadyR                   := false.B
            cfgStartR(i)                := false.B
            switch (cfgStateR) {
                is (ConfigState.Idle) {
                    cfgReadyR           := true.B
                    if (cfgFifo.io.deq.valid & cfgReadyR) {
                        cfgStartR(i)    := true.B
                        cfgStateR       := ConfigState.Running
                    }
                }
                is (ConfigState.Running) {
                    when (cfgLast(i)) {
                        cfgReadyR       := true.B
                        cfgStateR       := ConfigState.Idle
                    }
                }
            }
        }

        
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
        readDone3R  := readDone2R
        readAddr3R  := Cat(readAddr2R(addrBits-1, blockBytesLog2), 0.U(blockBytesLog2))
        readMask3R  := getMask(readAddr2R, readSize2R)

        readFifo.io.enq.valid = readValid3R
        readFifo.io.enq.bits  = Cat(readDone3R, readMask3R, readAddr3R)

        readFifo.io.deq.ready = arbFifo.io.enq.ready && !writeFifo.io.deq.valid
        writeFifo.io.deq.ready = arbFifo.io.enq.ready

        arbValidR := (writeFifo.io.deq.valid || readFifo.io.deq.valid) && arbFifo.io.enq.ready

        val tl_a_data = WireInit(0.U(dataBits.W))
        val tl_a_addr = WireInit(0.U(addrBits.W))
        val tl_a_mask = WireInit(0.U(blockBytes.W))

        when (writeFifo.io.deq.valid) {
        
        } .otherwise {
            tl_a_addr   := readFifo.io.deq.bits()
            arbDataR    := edge.Get(0.U, tl_a_addr, log2Ceil(blockBytes).U)._2
        }

        arbDataR := 

        switch (arbStateR) {
            when (readFifo.io.deq.valid) {
                
            }
        }

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

        when ()

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



        for (i <- 0 to (blockBytes-1))
        {
            val byteFifo             = Module(new Fifo(UInt(8.W), 8, 2))
            byteFifo.io.enq.valid   := tlFifoWrEnVecR(i) && tl_d_validR
            byteFifo.io.enq.bits    := (tlFifoWrDataR >> (8 * i))(7, 0)
            tlFifoWrReady(i)        := byteFifo.io.enq.ready

            byteFifoRdValid(i)      := byteFifo.io.deq.valid
            byteFifo.io.deq.ready   := byteFifoRdReady(i)
        }

        bytesLeftR      := bytesLeftR - beatBytes
        when (bytes)
        when (bytesLeftR <= beatBytes) {
            readStateR  :=  doneR
        }


        byteFifoRdReady := (byteFifoRdValid.asUInt === rdMaskR) ? rdMaskR : 0

        ramWrEnR                    := byteFifoRdReady
        ramWrDataR                  := byteFIfoRdData.asUInt

        when (byteFifoRdValid.asUInt === rdMaskR) {
            rdMaskR                 := rdMaskR >> 
        }
        
        when () (ramWrEnR)
        ramAddrR                    := 

        byFifoWrReadyR              := (byteFifoWrReady.asUInt === -1.S(blockBytes.W).asUInt)
        byteFifoRdDataR             := byteFifoRdData.asUInt

        when (tlFifo)
        switch(writeStateR) {
            is (WriteState.Init) {
                addrR               := 0
            }
            is (WriteState.Wait) {
                when (byteFifoRdReady == ) {

                }
                addrR               := addrR + 1
            }
        }


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
