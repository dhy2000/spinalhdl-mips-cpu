package component

import datapath.Bus._
import global.Const._
import instruction.Instruction
import spinal.core._

import scala.language.postfixOps

class Decoder extends Component {
  val io = new Decoder.IoBundle
  // when.elseWhen.elseWhen...
  io.valid := True
  switch(io.code) {
    Instruction.instrSet.foreach(inst => is(inst.format) {
      io.inst := inst.tag
    })
    default {
      io.inst := Instruction.Tag.nop
      io.valid := False
    }
  }
  io.field.rs := io.code(25 downto 21).asUInt
  io.field.rt := io.code(20 downto 16).asUInt
  io.field.rd := io.code(15 downto 11).asUInt
  io.field.shamt := io.code(10 downto 6).asUInt
  io.field.imm16 := io.code(15 downto 0).asUInt
  io.field.jump := io.code(25 downto 0).asUInt
}

object Decoder {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = Generate.target
    ).generate(new Decoder)
  }

  class IoBundle extends Bundle {
    val code: Bits = in Bits (Word.width bits)
    val inst: SpinalEnumCraft[Instruction.Tag.type] = out(Instruction.Tag())
    val field: Field = out(new Field)
    val valid: Bool = out Bool()
  }
}