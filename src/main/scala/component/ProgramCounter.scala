package component

import global.Const._
import spinal.core._

import scala.language.postfixOps

class ProgramCounter extends Component {
  val io = new ProgramCounter.IoBundle

  private val pc = Reg(UInt(Word.width bits)) init Address.text

  when(io.enable) {
    pc := Mux(io.branch, io.next, pc + 4)
  }

  io.current := pc
}

object ProgramCounter {
  class IoBundle extends Bundle {
    val enable: Bool = in Bool()
    val branch: Bool = in Bool()
    val next: UInt = in UInt (Word.width bits)
    val current: UInt = out UInt (Word.width bits)
  }
}
