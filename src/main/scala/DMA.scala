package cnnHwAccelerator

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._

case class DMAParams(
  address: BigInt = 0x4000)

case object DMAKey extends Field[Option[DMAParams]](None)

class DMAConfiguration(params: DMAParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule // ClockSinkDomain(ClockSinkParameters())(p)
{
    val device = new SimpleDevice("dma", Seq("cpu"))
    val node = TLRegisterNode(
        address = Seq(AddressSet(params.address, 4096-1)), 
        device = device,
        beatBytes = beatBytes)

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this)
    {
        val io = IO(new Bundle {
            val sourceAddr = Output(UInt(64.W))
            val destAddr   = Output(UInt(64.W))
            val size       = Output(UInt(32.W))
            val start      = Output(Bool())
        })

        val sourceAddr = RegInit(0.U(64.W))
        val destAddr   = RegInit(0.U(64.W))
        val size       = RegInit(0.U(32.W))
        val start      = Wire(new DecoupledIO(Bool()))
        start.ready   := true.B

        node.regmap(
            0x00 -> Seq(RegField(64, sourceAddr)),
            0x08 -> Seq(RegField(64, destAddr)),
            0x10 -> Seq(RegField(32, size)),
            0x14 -> Seq(RegField.w(1, start)))

        io.sourceAddr := sourceAddr
        io.destAddr   := destAddr
        io.size       := size
        io.start      := start.bits && start.valid
    }
}

object DMAReadState extends ChiselEnum
{
    val Init, Read, Wait = Value
}

object DMAWriteState extends ChiselEnum
{
    val Init, Wait, Write = Value
}

object DMAArbState extends ChiselEnum
{
    val Init, WaitAData, WaitAReady, WaitDData = Value
}

class DMAClient(beatBytes: Int)(implicit p: Parameters) extends LazyModule
{
    val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
        name = "dma-client",
        sourceId = IdRange(0, 1))))))

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this)
    {
        
        /* Ports */
        val io = IO(new Bundle {
            val sourceAddr = Input(UInt(64.W))
            val destAddr   = Input(UInt(64.W))
            val size       = Input(UInt(32.W))
            val start      = Input(Bool())
            val busy       = Output(Bool())
        })

        /* Parse Bus Input */
        val (tl, edge) = node.out(0)
        
        /* Constants */
        val blockBytes      = 8 // p(CacheBlockBytes)
        val blockBytesLog2  = log2Ceil(blockBytes)
        val dataBits        = 8 * blockBytes
        val addrBits        = edge.bundle.addressBits

        /* Functions */
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

        /* Read FIFO FSM signals */
        val readStateR          = RegInit(DMAReadState.Init)
        val readValid2R         = RegInit(false.B)
        val sourceAddrR         = Reg(UInt(addrBits.W))
        val numBytesReadR       = Reg(UInt((blockBytesLog2+1).W))
        val readSizeR           = Reg(UInt(32.W))

        /* Read FIFO Pipeline signals */
        val sourceAddr2R        = Reg(UInt(addrBits.W))
        val readMask2R          = Reg(UInt(blockBytes.W))

        /* Tilelink Buffer Signals */
        val tlFifoWrEnR         = Reg(UInt(blockBytes.W))
        val tlFifoWrDataR       = Reg(UInt(dataBits.W))
        val tlFifoWrReadyR      = RegInit(false.B)
        val tlFifoRdReadyR      = RegInit(0.U(blockBytes.W))
        val tlFifoRdDataR       = WireInit(0.U(dataBits.W))

        /* Write FIFO FSM Signals */
        val writeStateR         = RegInit(DMAWriteState.Init)
        val writeValid2R        = RegInit(false.B)
        val destAddrR           = Reg(UInt(addrBits.W))
        val writeMaskR          = Reg(UInt(blockBytes.W))
        val numBytesWriteR      = Reg(UInt((blockBytesLog2+1).W))
        val writeSizeR          = Reg(UInt(32.W))
        val nextWriteSizeR      = Reg(UInt(32.W))
        val nextDestAddrR       = Reg(UInt(addrBits.W))
        val lastWriteR          = Reg(Bool())

        /* Write FIFO Pipeline Signals */
        val writeMask2R         = Reg(UInt(blockBytes.W))
        val destAddr2R          = Reg(UInt(addrBits.W))

        /* Arbitrator FSM */
        val arbStateR           = RegInit(DMAArbState.Init)
        val readReadyR          = RegInit(false.B)
        val writeReadyR         = RegInit(false.B)
        val tl_d_validR         = RegInit(false.B)
        val busyR               = RegInit(false.B)
        val readShiftR          = Reg(UInt(blockBytesLog2.W))
        val tl_d_maskR          = Reg(UInt(blockBytes.W))
        val tl_d_lastR          = Reg(Bool())
        val tl_a_bitsR          = Reg(tl.a.bits.cloneType)
        val tl_a_validR         = RegInit(false.B)
        val tl_d_readyR         = RegInit(false.B)

        /* FIFO declarations */
        val readFifo                = Module(new Fifo(UInt((addrBits + blockBytes).W), 8, 1))
        val writeFifo               = Module(new Fifo(UInt((dataBits + addrBits + blockBytes + 1).W), 8, 1))

        /* Read FIFO FSM */
        readValid2R                 := false.B
        switch (readStateR) {
            is (DMAReadState.Init) {
                sourceAddrR         := io.sourceAddr(addrBits - 1, 0)
                numBytesReadR       := blockBytes.U - io.sourceAddr(blockBytesLog2 - 1, 0)
                readSizeR           := io.size
                when (io.start && !busyR) {
                    readStateR      := DMAReadState.Read
                }
            }
            is (DMAReadState.Read) {
                when (readFifo.io.enq.ready) {
                    readValid2R     := true.B
                    numBytesReadR   := blockBytes.U
                    sourceAddrR     := sourceAddrR + numBytesReadR
                    readSizeR       := readSizeR   - numBytesReadR
                    when (readSizeR <= numBytesReadR) {
                        readStateR  := DMAReadState.Init
                    }
                }
            }
        }

        /* Read FIFO Pipeline */
        sourceAddr2R                := Cat(sourceAddrR(addrBits - 1, blockBytesLog2), 0.U(blockBytesLog2.W))
        readMask2R                  := getMask(sourceAddrR, readSizeR)

        /* Read FIFO */
        readFifo.io.enq.valid       := readValid2R
        readFifo.io.enq.bits        := Cat(sourceAddr2R, readMask2R)
        readFifo.io.deq.ready       := readReadyR

        /* Tilelink Buffer Input Pipeline */
        tlFifoWrEnR                 := (Cat(tl_d_maskR, tl_d_maskR) >> readShiftR)(blockBytes-1, 0)
        tlFifoWrDataR               := (Cat(edge.data(tl.d.bits), edge.data(tl.d.bits)) >> (Cat(readShiftR, 0.U(3.W))))(dataBits-1, 0)

        /* Tilelink Buffer */
        val tlFifoWrReady           = VecInit(Seq.fill(blockBytes)(false.B))
        val tlFifoRdValid           = VecInit(Seq.fill(blockBytes)(false.B))
        val tlFifoRdDataVecR        = Reg(Vec(blockBytes, UInt(8.W)))
        val tlFifoWrEnVecR          = VecInit(tlFifoWrEnR.asBools)
        val tlFifoRdReadyVecR       = VecInit(tlFifoRdReadyR.asBools)

        for (i <- 0 to (blockBytes-1))
        {
            val tlFifo              = Module(new Fifo(UInt(8.W), 8, 2))
            tlFifo.io.enq.valid     := tlFifoWrEnVecR(i) && tl_d_validR
            tlFifo.io.enq.bits      := (tlFifoWrDataR >> (8 * i))(7, 0)
            tlFifoWrReady(i)        := tlFifo.io.enq.ready
            tlFifoRdValid(i)        := tlFifo.io.deq.valid
            tlFifo.io.deq.ready     := tlFifoRdReadyVecR(i)
            when (tlFifoRdReadyVecR(i)) {
                tlFifoRdDataVecR(i) := tlFifo.io.deq.bits
            }
        }
        tlFifoWrReadyR              := (tlFifoWrReady.asUInt === -1.S(blockBytes.W).asUInt)
        tlFifoRdDataR               := tlFifoRdDataVecR.asUInt

        /* Write FIFO FSM */
        writeValid2R                := false.B
        tlFifoRdReadyR              := 0.U
        switch (writeStateR) {
            is (DMAWriteState.Init) {
                destAddrR           := Cat(io.destAddr(addrBits-1, blockBytesLog2), 0.U(blockBytesLog2.W))
                writeMaskR          := getMask(io.destAddr, io.size)
                numBytesWriteR      := blockBytes.U - io.destAddr(blockBytesLog2-1, 0)
                writeSizeR          := io.size
                when (io.start && !busyR) {
                    writeStateR     := DMAWriteState.Wait
                }
            }
            is (DMAWriteState.Wait) {
                nextWriteSizeR      := writeSizeR - numBytesWriteR
                nextDestAddrR       := destAddrR + blockBytes.U
                lastWriteR          := writeSizeR <= numBytesWriteR
                when ((writeMaskR & tlFifoRdValid.asUInt) === writeMaskR) {
                    tlFifoRdReadyR  := writeMaskR
                    writeStateR     := DMAWriteState.Write
                }
            }
            is (DMAWriteState.Write) {
                when (writeFifo.io.enq.ready) {
                    writeValid2R    := true.B
                    numBytesWriteR  := blockBytes.U
                    writeSizeR      := nextWriteSizeR
                    destAddrR       := nextDestAddrR
                    writeMaskR      := getMask(nextDestAddrR, nextWriteSizeR)
                    when (lastWriteR) {
                        writeStateR := DMAWriteState.Init
                    } .otherwise {
                        writeStateR := DMAWriteState.Wait
                    }
                }
            }
        }

        /* Write FIFO Pipeline */
        writeMask2R                 := writeMaskR
        destAddr2R                  := destAddrR

        /* Write FIFO */
        writeFifo.io.enq.valid      := writeValid2R
        writeFifo.io.enq.bits       := Cat(lastWriteR, tlFifoRdDataR, destAddr2R, writeMask2R)
        writeFifo.io.deq.ready      := writeReadyR
       
        /* Arbitrator FSM */
        val tl_a_data               = WireInit(0.U(dataBits.W))
        val tl_a_addr               = WireInit(0.U(addrBits.W))
        val tl_a_mask               = WireInit(0.U(blockBytes.W))

        readReadyR                  := false.B
        writeReadyR                 := false.B
        tl_d_validR                 := false.B
        switch (arbStateR) {
            is (DMAArbState.Init) {
                readShiftR          := io.destAddr(blockBytesLog2 - 1, 0) - io.sourceAddr(blockBytesLog2 - 1, 0)
                when (io.start) {
                    arbStateR       := DMAArbState.WaitAData
                    busyR           := true.B
                }
            }
            is (DMAArbState.WaitAData) {
                tl_d_maskR          := readFifo.io.deq.bits(blockBytes-1, 0)
                tl_d_lastR          := false.B
                when (writeFifo.io.deq.valid) {
                    tl_d_lastR      := writeFifo.io.deq.bits(dataBits+addrBits+blockBytes)
                    tl_a_data       := writeFifo.io.deq.bits(dataBits+addrBits+blockBytes-1, addrBits+blockBytes)
                    tl_a_addr       := writeFifo.io.deq.bits(addrBits+blockBytes-1, blockBytes)
                    tl_a_mask       := writeFifo.io.deq.bits(blockBytes-1, 0)
                    tl_a_bitsR      := edge.Put(0.U, tl_a_addr, log2Ceil(blockBytes).U, tl_a_data, tl_a_mask)._2
                    tl_a_validR     := true.B
                    writeReadyR     := true.B
                    arbStateR       := DMAArbState.WaitAReady
                }
                .elsewhen (readFifo.io.deq.valid) {
                    tl_a_addr       := readFifo.io.deq.bits(addrBits+blockBytes-1, blockBytes)
                    tl_a_bitsR      := edge.Get(0.U, tl_a_addr, log2Ceil(blockBytes).U)._2
                    tl_a_validR     := true.B
                    readReadyR      := true.B
                    arbStateR       := DMAArbState.WaitAReady
                }
            }
            is (DMAArbState.WaitAReady) {
                when (tl.a.fire) {
                    tl_d_readyR     := true.B
                    tl_a_validR     := false.B
                    arbStateR       := DMAArbState.WaitDData
                }
            }
            is (DMAArbState.WaitDData) {
                when (tl.d.fire) {
                    tl_d_readyR     := false.B
                    tl_d_validR     := edge.hasData(tl.d.bits)
                    when (tl_d_lastR) {
                        busyR       := false.B
                        arbStateR   := DMAArbState.Init
                    } .otherwise {
                        arbStateR   := DMAArbState.WaitAData
                    }
                }
            }
        }

        tl.a.valid  := tl_a_validR
        tl.a.bits   := tl_a_bitsR
        tl.d.ready  := tl_d_readyR && tlFifoWrReadyR
        io.busy     := busyR
    }

}

class DMA(params: DMAParams, managerBeatBytes: Int, clientBeatBytes: Int)(implicit p: Parameters) extends LazyModule {

    val dmaConfig   = LazyModule(new DMAConfiguration(params, managerBeatBytes))
    val dmaClient   = LazyModule(new DMAClient(clientBeatBytes))

    val client      = TLIdentityNode()
    val manager     = TLIdentityNode()

    dmaConfig.node := manager
    client := dmaClient.node

    lazy val module = new Impl

    class Impl extends LazyModuleImp(this) {

        val io = IO(new Bundle{
            val busy = Output(Bool())
        })
        
        dmaClient.module.io.sourceAddr := dmaConfig.module.io.sourceAddr
        dmaClient.module.io.destAddr   := dmaConfig.module.io.destAddr
        dmaClient.module.io.size       := dmaConfig.module.io.size
        dmaClient.module.io.start      := dmaConfig.module.io.start

        io.busy := dmaClient.module.io.busy
    }
}

trait CanHavePeripheryDMA { this: BaseSubsystem =>
    private val managerName = "dma_manager"
    private val clientName  = "dma_client"
    private val pbus = locateTLBusWrapper(PBUS)
    private val fbus = locateTLBusWrapper(FBUS)
    
    val dma_busy = p(DMAKey) match {
        case Some(params) => {

            val domain = pbus.generateSynchronousDomain.suggestName("dma_domain")
            val dma = domain{LazyModule(new DMA(params, pbus.beatBytes, fbus.beatBytes)(p))}

            pbus.coupleTo(managerName) { dma.manager := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
            fbus.coupleFrom(clientName) { _ := dma.client }
	    
            val dma_busy = domain{ InModuleBody {
                val busy = IO(Output(Bool())).suggestName("dma_busy")
                busy := dma.module.io.busy
                busy
            } }

            Some(dma_busy)
        }
        case None => None
    }
}

class WithDMA extends Config((site, here, up) => {
  case DMAKey => {
    Some(DMAParams())
  }
})