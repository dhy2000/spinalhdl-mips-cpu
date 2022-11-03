package component

import global.Const._
import spinal.core._

import scala.language.postfixOps

class RegisterFile extends Component {
  val io = new RegisterFile.IoBundle
  noIoPrefix()

  private val reg = Vec(Reg(UInt(Word.width bits)) init 0, Register.num)

  io.readData1 := reg(io.readAddr1)
  io.readData2 := reg(io.readAddr2)

  when(io.writeEnable && io.writeAddr =/= 0) {
    reg(io.writeAddr) := io.writeData
  }

  // there is no forward inside RegisterFile
}

object RegisterFile {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog, targetDirectory = Generate.target).generate(new RegisterFile)
  }

  class IoBundle extends Bundle {
    val readAddr1: UInt = in UInt (Register.addrWidth bits)
    val readAddr2: UInt = in UInt (Register.addrWidth bits)
    val readData1: UInt = out UInt (Word.width bits)
    val readData2: UInt = out UInt (Word.width bits)

    val writeEnable: Bool = in Bool()
    val writeAddr: UInt = in UInt (Register.addrWidth bits)
    val writeData: UInt = in UInt (Word.width bits)
  }
}
