package cnnHwAccelerator

import sys.process._

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._

// import scala.collection.mutable.{ListBuffer}

case class CnnHwAcceleratorParams (
    address: BigInt = 0x4000,
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
    "MAX_SIZE"       -> IntParam(MAX_SIZE))) with HasBlackBoxPath {

    override def desiredName: String = "cnn_hw_accelerator"

    val io = IO(new CnnHwAcceleratorIO(
        BUS_ADDR_WIDTH,
        BUS_DATA_WIDTH,
        VECTOR_SIZE,
        MAX_SIZE))
    
    val chipyardDir = System.getProperty("user.dir")
    val vsrcDir     = s"$chipyardDir/generators/cnn-hw-accelerator/src/main/resources/vsrc"

    // val proc = s"make -C $vsrcDir -f AltMakefile default"
    // require(proc.! == 0, "Failed to run preprocessing step")

    addPath(s"$vsrcDir/cnn_hw_accelerator.preprocessed.v")
    // addResource("/vsrc/cnn_hw_accelerator.v")
}

class ReadController(sizeWidth: Int, addrWidth: Int, beatBytes: Int) extends Module
{
    val dataWidth  = 8 * beatBytes
    val shiftWidth = log2Ceil(beatBytes)

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

    def getMask(bytesLeft : UInt) : UInt = {
        val mask = VecInit(Seq.fill(beatBytes)(false.B))
        for (i<-0 to (beatBytes-1)) {
            when (bytesLeft > i.U)
            {
                mask(i) := true.B
            }
        }
        return mask.asUInt
    }

    object State extends ChiselEnum
    {
        val Init, Read = Value
    }

    val validR       = RegInit(false.B)
    val lastR        = RegInit(false.B)
    val stateR       = RegInit(State.Init)
    val shiftR       = Reg(UInt(shiftWidth.W))
    val bytesLeftR   = Reg(UInt(sizeWidth.W))
    val bytesReadR   = Reg(UInt((shiftWidth+1).W))
    val nextAddrR    = Reg(UInt(addrWidth.W))

    validR          := false.B
    lastR           := false.B
    switch (stateR) {
        is (State.Init) {
            when (io.startIn) {
                shiftR          := io.sourceAddrIn(shiftWidth-1, 0)
                bytesReadR      := beatBytes.U - io.sourceAddrIn(shiftWidth-1, 0)
                bytesLeftR      := io.sizeIn
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
                    lastR       := true.B
                    stateR      := State.Init
                }
            }
        }
    }

    val addrR        = Reg(UInt(addrWidth.W))
    val rdEnR        = Reg(UInt(beatBytes.W))

    addrR           := nextAddrR
    rdEnR           := getMask(bytesLeftR)

    val valid2R      = RegInit(false.B)
    val last2R       = RegInit(false.B)
    val addr2R       = Reg(UInt(addrWidth.W))
    val rdEn2R       = Reg(UInt(beatBytes.W))

    valid2R         := validR
    addr2R          := Cat(addrR(addrWidth-1, shiftWidth), 0.U(shiftWidth.W))
    rdEn2R          := rdEnR << shiftR
    last2R          := lastR

    io.rdValidOut   := valid2R
    io.rdEnOut      := rdEn2R
    io.addrOut      := addr2R
    io.lastOut      := last2R
}

class AlignmentBuffer(sizeWidth: Int, addrWidth: Int, beatBytes: Int) extends Module
{

    val dataWidth  = 8 * beatBytes
    val shiftWidth = log2Ceil(beatBytes)

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
        val wrDataOut    = Output(UInt(dataWidth.W))
        val addrOut      = Output(UInt(addrWidth.W))
        val lastOut      = Output(Bool())
    })

    def getMask(bytesLeft : UInt) : UInt = {
        val mask = VecInit(Seq.fill(beatBytes)(false.B))
        for (i<-0 to (beatBytes-1)) {
            when (bytesLeft > i.U)
            {
                mask(i) := true.B
            }
        }
        return mask.asUInt
    }

    object State extends ChiselEnum
    {
        val Init, Read = Value
    }

    val ctrlFifo     = Module(new Fifo(UInt((beatBytes+addrWidth+shiftWidth+1).W), 8, 3))

    val validR       = RegInit(false.B)
    val lastR        = RegInit(false.B)
    val stateR       = RegInit(State.Init)
    val shiftR       = Reg(UInt(shiftWidth.W))
    val bytesLeftR   = Reg(UInt(sizeWidth.W))
    val bytesReadR   = Reg(UInt((shiftWidth+1).W))
    val nextAddrR    = Reg(UInt(addrWidth.W))

    validR          := false.B
    lastR           := false.B
    switch (stateR) {
        is (State.Init) {
            when (io.startIn) {
                shiftR          := io.destAddrIn(shiftWidth-1, 0) - io.sourceAddrIn(shiftWidth-1, 0)
                bytesReadR      := beatBytes.U - io.destAddrIn(shiftWidth-1, 0)
                bytesLeftR      := io.sizeIn
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
                    lastR       := true.B
                    stateR      := State.Init
                }
            }
        }
    }

    val addrR    = Reg(UInt(addrWidth.W))
    val maskR    = Reg(UInt(beatBytes.W))

    addrR       := nextAddrR
    maskR       := getMask(bytesLeftR)

    val valid2R  = RegInit(false.B)
    val last2R   = RegInit(false.B)
    val addr2R   = Reg(UInt(addrWidth.W))
    val mask2R   = Reg(UInt(beatBytes.W))
    val shift2R  = Reg(UInt())

    valid2R     := validR
    addr2R      := Cat(addrR(addrWidth-1, shiftWidth), 0.U(shiftWidth.W))
    mask2R      := (maskR << addrR(shiftWidth-1, 0)) >> addrR(shiftWidth-1, 0)
    shift2R     := shiftR
    last2R      := lastR

    val valid3R  = RegInit(false.B)
    val addr3R   = Reg(UInt(addrWidth.W))
    val mask3R   = Reg(UInt(beatBytes.W))
    val shift3R  = Reg(UInt())
    val last3R   = Reg(Bool())
    
    valid3R     := valid2R
    addr3R      := addr2R
    mask3R      := (Cat(mask2R, mask2R) >> shift2R)(beatBytes-1, 0)
    shift3R     := shift2R
    last3R      := last2R 

    ctrlFifo.io.enq.bits    := Cat(last3R, shift3R, mask3R, addr3R)
    ctrlFifo.io.enq.valid   := valid3R

    val addrLo      = 0
    val addrHi      = addrLo + addrWidth - 1
    val maskLo      = addrHi + 1
    val maskHi      = maskLo + beatBytes - 1
    val shiftLo     = maskHi + 1
    val shiftHi     = shiftLo + shiftWidth - 1
    val lastIdx     = shiftHi + 1
    
    val byteFifoRdReady = WireInit(0.U(beatBytes.W))
    val byteFifoWrReady = VecInit(Seq.fill(beatBytes)(false.B))
    val byteFifoRdValid = VecInit(Seq.fill(beatBytes)(false.B))
    val byteFifoRdData  = VecInit(Seq.fill(beatBytes)(0.U(8.W)))

    for (i <- 0 to (beatBytes-1)) {
        val byteFifo             = Module(new Fifo(UInt(8.W), 8, 2))
        byteFifo.io.enq.valid   := io.wrValidIn(i)
        byteFifo.io.enq.bits    := (io.wrDataIn >> (8 * i))(7, 0)
        byteFifoWrReady(i)      := byteFifo.io.enq.ready
        byteFifo.io.deq.ready   := byteFifoRdReady(i) 
        byteFifoRdValid(i)      := byteFifo.io.deq.valid
        byteFifoRdData(i)       := byteFifo.io.deq.bits
    }
    
    val wrReadyR     = RegInit(false.B)
    wrReadyR        := (byteFifoWrReady.asUInt === -1.S(beatBytes.W).asUInt)

    io.wrReadyOut   := wrReadyR

    val rdReady      = WireInit(false.B)
    rdReady         := io.wrReadyIn & ctrlFifo.io.deq.valid & (ctrlFifo.io.deq.bits(maskHi, maskLo) === byteFifoRdValid.asUInt)

    ctrlFifo.io.deq.ready := rdReady

    when (rdReady) {
        byteFifoRdReady := ctrlFifo.io.deq.bits(maskHi, maskLo)
    }

    val wrEnOutR         = RegInit(0.U(beatBytes.W))
    val wrDataOutR       = Reg(UInt(dataWidth.W))
    val addrOutR         = Reg(UInt(addrWidth.W))
    val shiftOutR        = Reg(UInt(shiftWidth.W))
    val lastOutR         = RegInit(false.B)

    wrEnOutR            := byteFifoRdReady
    wrDataOutR          := byteFifoRdData.asUInt
    addrOutR            := ctrlFifo.io.deq.bits(addrHi, addrLo)
    shiftOutR           := ctrlFifo.io.deq.bits(shiftHi, shiftLo)
    lastOutR            := ctrlFifo.io.deq.bits(lastIdx) & rdReady

    val wrEnOut2R        = RegInit(0.U(beatBytes.W))
    val wrDataOut2R      = Reg(UInt(dataWidth.W))
    val addrOut2R        = Reg(UInt(addrWidth.W))
    val lastOut2R        = RegInit(false.B)

    wrEnOut2R           := (Cat(wrEnOutR, wrEnOutR) >> shiftOutR)(beatBytes-1, 0)
    wrDataOut2R         := (Cat(wrDataOutR, wrDataOutR) >> Cat(shiftOutR, 0.U(3.W)))(dataWidth-1, 0)
    addrOut2R           := addrOutR
    lastOut2R           := lastOutR
    
    io.wrEnOut          := wrEnOut2R
    io.wrDataOut        := wrDataOut2R
    io.addrOut          := addrOut2R
    io.lastOut          := lastOut2R
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
            val dataAddrOut   = Output(UInt(64.W))
            val dataColsOut   = Output(UInt(32.W))
            val dataRowsOut   = Output(UInt(32.W))
            val filtAddrOut   = Output(UInt(64.W))
            val filtColsOut   = Output(UInt(32.W))
            val filtRowsOut   = Output(UInt(32.W))
            val destAddrOut   = Output(UInt(64.W))
            val startOut      = Output(Bool())
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

        io.dataAddrOut  := dataAddr
        io.dataColsOut  := dataCols
        io.dataRowsOut  := dataRows
        io.filtAddrOut  := filtAddr
        io.filtColsOut  := filtCols
        io.filtRowsOut  := filtRows
        io.destAddrOut  := destAddr
        io.startOut     := start.bits && start.valid
    }
}

class CnnHwAcceleratorClient(params: CnnHwAcceleratorParams, beatBytesUpdate: Int)(implicit p: Parameters) extends LazyModule
{
    val beatBytes = 8

    val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
        name = "cnn-hw-accelerator-client",
        sourceId = IdRange(0, 1))))))

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this)
    {
        
        val io = IO(new Bundle {
            val dataAddrIn = Input(UInt(64.W))
            val dataColsIn = Input(UInt(32.W))
            val dataRowsIn = Input(UInt(32.W))
            val filtAddrIn = Input(UInt(64.W))
            val filtColsIn = Input(UInt(32.W))
            val filtRowsIn = Input(UInt(32.W))
            val destAddrIn = Input(UInt(64.W))
            val startIn    = Input(Bool())
            val doneOut    = Output(Bool())
        })

        val (tl, edge) = node.out(0)
    
        // Bus Constants
        val busAddrWidth = edge.bundle.addressBits
        val busDataWidth = 8*beatBytes

        // Hardware Accelerator Constants
        val dataWidth   = 32
        val dataBytes   = dataWidth/8
        val dimWidth    = log2Ceil(params.maxSize) + 1
        val addrWidth   = log2Ceil(params.maxSize) + log2Ceil(dataBytes)
        val countWidth  = addrWidth + 1 
        val shiftWidth  = log2Ceil(beatBytes/dataBytes)

        // Read FIFO Configuration
        val rdFifoEnLo   = 0
        val rdFifoEnHi   = rdFifoEnLo + beatBytes - 1
        val rdFifoAddrLo = rdFifoEnHi + 1
        val rdFifoAddrHi = rdFifoAddrLo + busAddrWidth - 1
        val rdFifoWidth  = rdFifoAddrHi + 1

        // Write FIFO Configuration
        val wrFifoDataLo  = 0
        val wrFifoDataHi  = wrFifoDataLo + busDataWidth - 1
        val wrFifoEnLo    = wrFifoDataHi + 1
        val wrFifoEnHi    = wrFifoEnLo + beatBytes - 1
        val wrFifoAddrLo  = wrFifoEnHi + 1
        val wrFifoAddrHi  = wrFifoAddrLo + busAddrWidth - 1
        val wrFifoLastIdx = wrFifoAddrHi + 1
        val wrFifoWidth   = wrFifoLastIdx + 1

        // Arbitrator FSM
        object ArbState extends ChiselEnum
        {
            val Idle, WaitA, WaitD = Value
        }

        // Input Pipeline #1
        val startR      = RegInit(false.B)
        val dataSizeR   = Reg(UInt(countWidth.W))
        val filtSizeR   = Reg(UInt(countWidth.W))
        val resultColsR = Reg(UInt(dimWidth.W))
        val resultRowsR = Reg(UInt(dimWidth.W))
        val dataAddrR   = Reg(UInt(busAddrWidth.W))
        val filtAddrR   = Reg(UInt(busAddrWidth.W))
        val destAddrR   = Reg(UInt(busAddrWidth.W))

        // Input Pipeline #2
        val start2R      = RegInit(false.B)
        val dataSize2R   = Reg(UInt(countWidth.W))
        val filtSize2R   = Reg(UInt(countWidth.W))
        val resultSize2R = Reg(UInt(countWidth.W))
        val dataAddr2R   = Reg(UInt(busAddrWidth.W))
        val filtAddr2R   = Reg(UInt(busAddrWidth.W))
        val destAddr2R   = Reg(UInt(busAddrWidth.W))

        // Read Controller
        val rdCtrlStartR = RegInit(false.B)
        val rdCtrlValidR = Reg(UInt(2.W))
        val rdCtrlAddrR  = Reg(Vec(2, UInt(busAddrWidth.W)))
        val rdCtrlSizeR  = Reg(Vec(2, UInt(countWidth.W)))

        // Read Alignment
        val rdAlignStartR  = RegInit(false.B)
        val rdAlignValidR  = Reg(UInt(2.W))
        val rdAlignSourceR = Reg(Vec(2, UInt(busAddrWidth.W)))
        val rdAlignSizeR   = Reg(Vec(2, UInt(countWidth.W)))

        // Hardware Accelerator
        val accStartR      = RegInit(false.B)
        val lastStickyR    = RegInit(false.B)
        val filtColsR      = Reg(UInt(dimWidth.W))
        val filtRowsR      = Reg(UInt(dimWidth.W))
        val dataColsR      = Reg(UInt(dimWidth.W))
        val dataRowsR      = Reg(UInt(dimWidth.W))

        // Write Alignment
        val accWrValid     = WireInit(false.B)
        val accWrEn        = WireInit(0.U(dataBytes.W))
        val accWrData      = WireInit(0.U(dataWidth.W))
        val accWrEnR       = RegInit(0.U(beatBytes.W))
        val accWrDataR     = Reg(UInt(busDataWidth.W))
        val writeShiftR    = Reg(UInt(shiftWidth.W))

        // Arbitrator
        val doneR          = RegInit(false.B)
        val arbStateR      = RegInit(ArbState.Idle)

        val tl_a_data      = WireInit(0.U(busDataWidth.W))
        val tl_a_addr      = WireInit(0.U(busAddrWidth.W))
        val tl_a_mask      = WireInit(0.U(beatBytes.W)) 
        val tl_a_bitsR     = Reg(tl.a.bits.cloneType)
        val tl_a_validR    = RegInit(false.B)
        val tl_d_readyR    = RegInit(false.B)
        val tl_d_validR    = Reg(UInt(beatBytes.W))
        val tl_d_dataR     = Reg(UInt(busDataWidth.W))
        val tl_d_maskR     = Reg(UInt(beatBytes.W))
        val tl_d_lastR     = Reg(Bool())
        
        val wrFifoReadyR   = RegInit(false.B)
        val rdFifoReadyR   = RegInit(false.B)

        // Read Controller
        val readCtrl = Module(new ReadController(countWidth, busAddrWidth, beatBytes))

        // Read Alignment Buffer
        val readAlign = Module(new AlignmentBuffer(countWidth, busAddrWidth, beatBytes))

        // Read FIFO
        val readFifo    = Module(new Fifo(UInt(rdFifoWidth.W), 8, 2))

        // Hardware Accelerator
        val accelerator = Module(new CnnHwAcceleratorBlackBox(busAddrWidth, busDataWidth, params.vectorSize, params.maxSize))

        // Write Alignment Buffer        
        val writeAlign = Module(new AlignmentBuffer(countWidth, busAddrWidth, beatBytes))

        // Write FIFO
        val writeFifo   = Module(new Fifo(UInt(wrFifoWidth.W), 8, 2))

        // Input Pipeline #1
        startR          := io.startIn
        dataSizeR       := Cat(io.dataColsIn(dimWidth-1, 0) * io.dataRowsIn(dimWidth-1, 0), 0.U(log2Ceil(dataBytes).W))
        filtSizeR       := Cat(io.filtColsIn(dimWidth-1, 0) * io.filtRowsIn(dimWidth-1, 0), 0.U(log2Ceil(dataBytes).W))
        resultColsR     := io.dataColsIn(dimWidth-1, 0) - io.filtColsIn(dimWidth-1, 0) + 1.U
        resultRowsR     := io.dataRowsIn(dimWidth-1, 0) - io.filtRowsIn(dimWidth-1, 0) + 1.U
        dataAddrR       := io.dataAddrIn(busAddrWidth-1, 0)
        filtAddrR       := io.filtAddrIn(busAddrWidth-1, 0)
        destAddrR       := io.destAddrIn(busAddrWidth-1, 0)

        // Input Pipeline #2
        start2R         := startR
        dataSize2R      := dataSizeR
        filtSize2R      := filtSizeR
        resultSize2R    := Cat(resultColsR * resultRowsR, 0.U(log2Ceil(dataBytes).W))
        dataAddr2R      := dataAddrR
        filtAddr2R      := filtAddrR
        destAddr2R      := destAddrR

        // Read Control Pipeline
        rdCtrlStartR    := false.B
        when (start2R) {
            rdCtrlStartR        := true.B
            rdCtrlValidR        := 3.U
            rdCtrlAddrR(0)      := dataAddr2R
            rdCtrlAddrR(1)      := filtAddr2R
            rdCtrlSizeR(0)      := dataSize2R
            rdCtrlSizeR(1)      := filtSize2R
        } .elsewhen (readCtrl.io.lastOut) {
            rdCtrlValidR        := rdCtrlValidR >> 1
            rdCtrlAddrR(0)      := rdCtrlAddrR(1)
            rdCtrlSizeR(0)      := rdCtrlSizeR(1)
            when (rdCtrlValidR(1)) {
                rdCtrlStartR    := true.B
            }
        }

        // Read Controller Connections
        // Configuration Pipeline
        readCtrl.io.startIn         := rdCtrlStartR
        readCtrl.io.sizeIn          := rdCtrlSizeR(0)
        readCtrl.io.sourceAddrIn    := rdCtrlAddrR(0)

        // Read FIFO
        readCtrl.io.rdReadyIn       := readFifo.io.enq.ready
        readFifo.io.enq.valid       := readCtrl.io.rdValidOut
        readFifo.io.enq.bits        := Cat(readCtrl.io.addrOut, readCtrl.io.rdEnOut)
        
        // Read Alignment Buffer Configuration Pipeline
        rdAlignStartR               := false.B
        when (start2R) {
            rdAlignStartR           := true.B
            rdAlignValidR           := 3.U
            rdAlignSourceR(0)       := dataAddr2R
            rdAlignSourceR(1)       := filtAddr2R
            rdAlignSizeR(0)         := dataSize2R
            rdAlignSizeR(1)         := filtSize2R
        } .elsewhen (readAlign.io.lastOut) {
            rdAlignValidR           := rdAlignValidR >> 1.U
            rdAlignSourceR(0)       := rdAlignSourceR(1)
            rdAlignSizeR(0)         := rdAlignSizeR(1)
            when (rdAlignValidR(1)) { 
                rdAlignStartR       := true.B
            }
        }

        // Read Alignment Buffer Configuration
        readAlign.io.startIn        := rdAlignStartR
        readAlign.io.sizeIn         := rdAlignSizeR(0)
        readAlign.io.destAddrIn     := Cat(0.U((busAddrWidth-countWidth).W), !rdAlignValidR(1), 0.U((countWidth-1).W))
        readAlign.io.sourceAddrIn   := rdAlignSourceR(0)

        // Connect Bus to Read Alignment Buffer
        readAlign.io.wrValidIn      := tl_d_validR
        readAlign.io.wrDataIn       := tl_d_dataR
        readAlign.io.wrReadyIn      := true.B

        // Connect Hardware Accelerator to Read Alignment Buffer
        // Compute Hardware Accelerator Start Signal
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
        when (io.startIn) {
            filtColsR               := io.filtColsIn(dimWidth-1, 0)
            filtRowsR               := io.filtRowsIn(dimWidth-1, 0)
            dataColsR               := io.dataColsIn(dimWidth-1, 0)
            dataRowsR               := io.dataRowsIn(dimWidth-1, 0)
        }
        
        // Accelerator clock and reset
        // Connect to implicit clock and reset
        accelerator.io.clkIn        := clock
        accelerator.io.rstIn        := reset

        // Source Connections
        accelerator.io.filtRowsIn   := filtRowsR
        accelerator.io.filtColsIn   := filtColsR
        accelerator.io.dataRowsIn   := dataRowsR
        accelerator.io.dataColsIn   := dataColsR
        
        accelerator.io.startIn      := accStartR
        accelerator.io.wrEnIn       := readAlign.io.wrEnOut
        accelerator.io.wrDataIn     := readAlign.io.wrDataOut
        accelerator.io.addrIn       := readAlign.io.addrOut

        // Sink Connections
        accelerator.io.readyIn      := writeAlign.io.wrReadyOut
        accWrValid                  := accelerator.io.validOut & writeAlign.io.wrReadyOut
        when (io.startIn) {
            writeShiftR             := 0.U
        } .elsewhen (accWrValid) {
            writeShiftR             := writeShiftR + 1.U
        }

        when (accWrValid) {
            accWrEn                 := -1.S(dataBytes.W).asUInt
        } .otherwise {
            accWrEn                 := 0.U(dataBytes.W)
        }

        accWrData                   := accelerator.io.dataOut

        accWrEnR                    := Cat(0.U((beatBytes    - dataBytes).W), accWrEn  ) << Cat(writeShiftR, 0.U(log2Ceil(dataBytes).W))
        accWrDataR                  := Cat(0.U((busDataWidth - dataWidth).W), accWrData) << Cat(writeShiftR, 0.U(log2Ceil(dataWidth).W))

        // Write Alignment Buffer
        writeAlign.io.startIn       := start2R
        writeAlign.io.sizeIn        := resultSize2R
        writeAlign.io.destAddrIn    := destAddr2R
        writeAlign.io.sourceAddrIn  := 0.U
        writeAlign.io.wrValidIn     := accWrEnR
        writeAlign.io.wrDataIn      := accWrDataR

        writeAlign.io.wrReadyIn     := writeFifo.io.enq.ready
        writeFifo.io.enq.valid      := writeAlign.io.wrEnOut =/= 0.U
        writeFifo.io.enq.bits       := Cat(writeAlign.io.lastOut, 
                                           writeAlign.io.addrOut,
                                           writeAlign.io.wrEnOut,
                                           writeAlign.io.wrDataOut)

        // Arbitrator FSM                     
        doneR                       := false.B
        wrFifoReadyR                := false.B
        rdFifoReadyR                := false.B
        tl_d_validR                 := 0.U
        switch (arbStateR) {
            is (ArbState.Idle) {
                tl_d_lastR          := false.B
                tl_d_maskR          := 0.U
                when (writeFifo.io.deq.valid) {
                    tl_d_lastR      := writeFifo.io.deq.bits(wrFifoLastIdx)
                    tl_a_data       := writeFifo.io.deq.bits(wrFifoDataHi, wrFifoDataLo)
                    tl_a_addr       := writeFifo.io.deq.bits(wrFifoAddrHi, wrFifoAddrLo)
                    tl_a_mask       := writeFifo.io.deq.bits(wrFifoEnHi, wrFifoEnLo)
                    tl_a_bitsR      := edge.Put(0.U, tl_a_addr, log2Ceil(beatBytes).U, tl_a_data, tl_a_mask)._2
                    tl_a_validR     := true.B
                    wrFifoReadyR    := true.B
                    arbStateR       := ArbState.WaitA
                } .elsewhen (readFifo.io.deq.valid) {
                    tl_d_maskR      := readFifo.io.deq.bits(rdFifoEnHi, rdFifoEnLo)
                    tl_a_addr       := readFifo.io.deq.bits(rdFifoAddrHi, rdFifoAddrLo)
                    tl_a_bitsR      := edge.Get(0.U, tl_a_addr, log2Ceil(beatBytes).U)._2
                    tl_a_validR     := true.B
                    rdFifoReadyR    := true.B
                    arbStateR       := ArbState.WaitA
                }
            }
            is (ArbState.WaitA) {
                when (tl.a.fire) {
                    tl_a_validR     := false.B
                    tl_d_readyR     := true.B
                    arbStateR       := ArbState.WaitD
                }
            }
            is (ArbState.WaitD) {
                when (tl.d.fire) {
                    tl_d_readyR     := false.B
                    tl_d_validR     := tl_d_maskR
                    arbStateR       := ArbState.Idle
                    when (tl_d_lastR) {
                        doneR       := true.B
                    }
                }
            }
        }

        tl.a.valid                  := tl_a_validR
        tl.a.bits                   := tl_a_bitsR
        tl.d.ready                  := tl_d_readyR && readAlign.io.wrReadyOut
        tl_d_dataR                  := edge.data(tl.d.bits)
        io.doneOut                  := doneR

        readFifo.io.deq.ready       := rdFifoReadyR
        writeFifo.io.deq.ready      := wrFifoReadyR
    }
}

class CnnHwAccelerator(params: CnnHwAcceleratorParams, managerBeatBytes: Int, clientBeatBytes: Int)(implicit p: Parameters) extends LazyModule {

    val accManager  = LazyModule(new CnnHwAcceleratorManager(params, managerBeatBytes))
    val accClient   = LazyModule(new CnnHwAcceleratorClient(params, clientBeatBytes))

    val client      = TLIdentityNode()
    val manager     = TLIdentityNode()

    accManager.node := manager
    client := accClient.node

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this) {

        val io = IO(new Bundle{
            val doneOut = Output(Bool())
        })
        
        accClient.module.io.dataAddrIn   := accManager.module.io.dataAddrOut
        accClient.module.io.dataColsIn   := accManager.module.io.dataColsOut
        accClient.module.io.dataRowsIn   := accManager.module.io.dataRowsOut
        accClient.module.io.filtAddrIn   := accManager.module.io.filtAddrOut
        accClient.module.io.filtColsIn   := accManager.module.io.filtColsOut
        accClient.module.io.filtRowsIn   := accManager.module.io.filtRowsOut
        accClient.module.io.destAddrIn   := accManager.module.io.destAddrOut
        accClient.module.io.startIn      := accManager.module.io.startOut

        io.doneOut := accClient.module.io.doneOut
    }
}

trait CanHaveCnnHwAccelerator { this: BaseSubsystem =>
    private val managerName = "cnn_hw_accelerator_manager"
    private val clientName  = "cnn_hw_accelerator_client"
    private val pbus = locateTLBusWrapper(PBUS)
    private val fbus = locateTLBusWrapper(FBUS)
    
    val accelerator_done = p(CnnHwAcceleratorKey) match {
        case Some(params) => {

            val domain = pbus.generateSynchronousDomain.suggestName("cnn_hw_accelerator_domain")
            val accelerator = domain{LazyModule(new CnnHwAccelerator(params, pbus.beatBytes, fbus.beatBytes)(p))} // fbus.beatBytes)(p))}

            pbus.coupleTo(managerName) { accelerator.manager := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
            fbus.coupleFrom(clientName) { _ := accelerator.client }
	    
            val accelerator_done = domain{ InModuleBody {
                val done = IO(Output(Bool())).suggestName("accelerator_done")
                done := accelerator.module.io.doneOut
                done
            } }

            Some(accelerator_done)
        }
        case None => None
    }
}

class WithCnnHwAccelerator extends Config((site, here, up) => {
  case CnnHwAcceleratorKey => {
    Some(CnnHwAcceleratorParams())
  }
})