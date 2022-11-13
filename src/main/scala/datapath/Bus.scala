package datapath

import global.Const._
import instruction.Instruction
import spinal.core._

import scala.language.postfixOps

object Bus {
  // F
  class Instr extends Bundle {
    val addr: UInt = UInt(Word.width bits)
    val code: Bits = Bits(Word.width bits)
    val inst: SpinalEnumCraft[Instruction.Tag.type] = Instruction.Tag()
  }

  object Instr {
    def apply(): Instr = {
      val zero = new Instr
      zero.addr := 0
      zero.code := 0
      zero.inst := Instruction.Tag.nop
      zero
    }
  }

  // D
  class Field extends Bundle {
    val rs: UInt = UInt(5 bits)
    val rt: UInt = UInt(5 bits)
    val rd: UInt = UInt(5 bits)
    val imm16: UInt = UInt(16 bits)
    val jump: UInt = UInt(26 bits)
    val shamt: UInt = UInt(5 bits)
  }

  object Field {
    def apply(): Field = {
      val zero = new Field
      zero.rs := 0
      zero.rt := 0
      zero.rd := 0
      zero.imm16 := 0
      zero.jump := 0
      zero.shamt := 0
      zero
    }
  }

  // Two per instruction
  class RegUse extends Bundle {
    val use: Bool = Bool()
    val addr: UInt = UInt(Register.addrWidth bits)
    val data: UInt = UInt(Word.width bits)
    val at: UInt = UInt(8 bits)
  }

  object RegUse {
    def apply(): RegUse = {
      val zero = new RegUse
      zero.use := False
      zero.addr := 0
      zero.data := 0
      zero.at := 0
      zero
    }

    def apply(addr: UInt, data: UInt): RegUse = {
      val regUse = new RegUse
      regUse.use := False
      regUse.addr := addr
      regUse.data := data
      regUse.at := 0
      regUse
    }
  }

  // Branch
  class NextInstr extends Bundle {
    val branch: Bool = Bool()
    val pc: UInt = UInt(Word.width bits)
  }

  object NextInstr {
    def apply(): NextInstr = {
      val zero = new NextInstr
      zero.branch := False
      zero.pc := 0
      zero
    }
  }

  class RegNew extends Bundle {
    val enable: Bool = Bool()
    val addr: UInt = UInt(Register.addrWidth bits)
    val data: UInt = UInt(Word.width bits)
    val new_at: UInt = UInt(8 bits)
    // one-hot encoding, to deal with undirected register writing
    val may_new: Bits = Bits(Register.num bits)
  }

  object RegNew {
    def apply(): RegNew = {
      val zero = new RegNew
      zero.enable := False
      zero.addr := 0
      zero.data := 0
      zero.new_at := 0
      zero.may_new := 0
      zero
    }
  }

  // E
  class MemAddress extends Bundle {
    val addr: UInt = UInt(Word.width bits)
    val byteEn: Bits = Bits(Word.byteCount bits)
  }

  object MemAddress {
    def apply(): MemAddress = {
      val zero = new MemAddress
      zero.addr := 0
      zero.byteEn := 0
      zero
    }
  }

  // M
  class MemLoad extends Bundle {
    val data: UInt = UInt(Word.width bits)
  }

  object MemLoad {
    def apply(): MemLoad = {
      val zero = new MemLoad
      zero.data := 0
      zero
    }
  }

  class MemStore extends Bundle {
    val data: UInt = UInt(Word.width bits)
  }

  object MemStore {
    def apply(): MemStore = {
      val zero = new MemStore
      zero.data := 0
      zero
    }
  }

  // W
}
