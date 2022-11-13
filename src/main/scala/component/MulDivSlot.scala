package component

import global.Const._
import spinal.core._

import scala.language.postfixOps

class MulDivSlot extends Component {
  // only simulate the delay slot of multiply/divide instruction
  val io = new MulDivSlot.IoBundle
  noIoPrefix()

  private val hi, lo = Reg(UInt(Word.width bits)) init 0
  private val count = Reg(UInt(8 bits)) init 0

  io.data.hi := Mux(count === 0, hi, U"0")
  io.data.lo := Mux(count === 0, lo, U"0")
  io.status.busy := count > 0
  io.status.count := count

  when(count > 0) {
    count := count - 1
  }.elsewhen(io.input.start) {
    count := io.input.delay
    hi := io.input.hi
    lo := io.input.lo
  }
}

object MulDivSlot {
  class Input extends Bundle {
    val hi, lo: UInt = UInt(Word.width bits)  // expected mul/div result
    val start: Bool = Bool()
    val delay: UInt = UInt(8 bits)
  }

  class Status extends Bundle {
    val busy: Bool = Bool()
    val count: UInt = UInt(8 bits)
  }

  class Data extends Bundle {
    val hi, lo: UInt = UInt(Word.width bits)  // output mul/div result
  }

  class IoBundle extends Bundle {
    val input: Input = in(new Input)
    val status: Status = out(new Status)
    val data: Data = out(new Data)
  }
}
