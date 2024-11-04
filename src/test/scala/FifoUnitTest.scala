package cnnHwAccelator

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FifoModule[T <: Data](ioType: T, depth: Int, skid: Int) extends Module {
    val in = IO(Flipped(Decoupled(ioType)))
    val out = IO(Decoupled(ioType))
    out <> Fifo(in, depth, skid)
}

class FifoUnitTest extends AnyFlatSpec with ChiselScalatestTester
{
    behavior.of("Fifo")

    it should "do stuff" in {
        test(new FifoModule(UInt(32.W), 8, 2)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            var wrData  = Array(0, 0)
            var wrValid = Array(false, false)
            var rdData  = 0
            for (i <- 0 until 256)
            {
                c.in.valid.poke(wrValid(1))
                c.in.bits.poke (wrData (1))
                if (i > 32)
                {
                    c.out.ready.poke(true.B)
                }
                else
                {
                    c.out.ready.poke(false.B)
                }
                wrValid(1) = wrValid(0)
                wrValid(0) = false
                wrData(1)  = wrData(0)
                if (c.in.ready.peek().litValue == 1)
                {
                    if (wrData(0) < 32)
                    {
                        wrValid(0) = true
                        wrData (0) = wrData(0) + 1
                    }
                }
                if (c.out.ready.peek().litValue == 1 && c.out.valid.peek().litValue == 1)
                {
                    rdData = rdData + 1
                    assert(c.out.bits.peek().litValue == rdData)
                }
                c.clock.step()
            }
            assert(rdData == 32)
        }
    }
}