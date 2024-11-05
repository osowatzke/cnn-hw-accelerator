package cnnHwAccelerator

import chisel3._
import chisel3.util._

class Fifo[T <: Data](val gen: T, val depth : Int, val skid : Int = 0) extends Module
{
    val cntWidth = log2Ceil(depth + 1)
    val maxCount = depth - skid

    /** The I/O for this queue */
    val io = IO(new QueueIO(gen, depth))

    val q = Module(new Queue(gen, depth))

    io.count := q.io.count

    val full = RegInit(false.B)

    io.enq.ready   := !full
    q.io.enq.valid := io.enq.valid
    q.io.enq.bits  := io.enq.bits
    
    assert(!q.io.enq.valid || q.io.enq.ready)

    io.deq <> q.io.deq
    // q.io.deq.ready <> io.deq.ready
    // io.deq.valid   <> q.io.deq.valid
    // io.deq.bits    <> q.io.deq.bits

    val do_enq = io.enq.valid
    val do_deq = io.deq.ready && io.deq.valid

    when (io.count > maxCount.U)
    {
        full := true.B
    }
    .elsewhen (io.count === maxCount.U)
    {
        when (do_deq && !do_enq)
        {
            full := false.B
        }
        .otherwise
        {
            full := true.B
        }
    }
    .elsewhen (io.count === (maxCount - 1).U)
    {
        when (!do_deq && do_enq)
        {
            full := true.B
        }
        .otherwise
        {
            full := false.B
        }
    }
    .otherwise
    {
        full := false.B
    }
    // full := (io.count > maxCount.U) || ((io.count === maxCount.U) && (!do_deq || do_enq))
}

object Fifo
{
    def apply[T <: Data](enq: DecoupledIO[T], depth: Int, skid : Int = 0): DecoupledIO[T] =
    {
        val q = Module(new Fifo(enq.bits.cloneType, depth, skid))
        q.io.enq.valid := enq.valid
        q.io.enq.bits  := enq.bits
        enq.ready      := q.io.enq.ready
        q.io.deq
    }
}