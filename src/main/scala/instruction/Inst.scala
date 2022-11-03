package instruction

import component._
import datapath.Data._
import global.Const._
import spinal.core._

import scala.language.postfixOps

abstract class Inst {
  val opcode: MaskedLiteral
  val tag: SpinalEnumElement[Inst.Tag.type]

  val use1_at: Int // 0 means not use
  val use2_at: Int // 0 means not use
  val new_at: Int // 0 means not new

  // stage d
  def br(inst: Instr, field: Field, use1: RegUse, use2: RegUse, ni: NextInstr): Unit = {
    ni.branch := False
    ni.pc := 0
  }

  def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {}

  def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew,
        mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {}

  def m(inst: Instr, field: Field, use1: RegUse, use2: RegUse, m_new: RegNew, w_new: RegNew,
        mem_addr: MemAddress, mem_store: MemStore): Unit = {}

  def w(inst: Instr, field: Field, use1: RegUse, use2: RegUse, w_new: RegNew, wr: RegNew,
        mem_addr: MemAddress, mem_store: MemStore, mem_load: MemLoad): Unit = {}
}

object Inst {
  val instrSet: List[Inst] = List(
    new Inst {
      override val opcode: MaskedLiteral = M"00000000000000000000000000000000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.nop
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = 0
    }, // nop
    new Inst {
      override val opcode: MaskedLiteral = M"000000---------------00000100000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.add
      override val use1_at: Int = Pipeline.Stage.E
      override val use2_at: Int = Pipeline.Stage.E
      override val new_at: Int = Pipeline.Stage.M

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := field.rd
      }

      override def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew, mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        m_new.data := use1.data + use2.data
      }
    }, // add
    new Inst {
      override val opcode: MaskedLiteral = M"000000---------------00000100010"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sub
      override val use1_at: Int = Pipeline.Stage.E
      override val use2_at: Int = Pipeline.Stage.E
      override val new_at: Int = Pipeline.Stage.M

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := field.rd
      }

      override def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew, mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        m_new.data := use1.data - use2.data
      }
    }, // sub
    new Inst {
      override val opcode: MaskedLiteral = M"001101--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.ori
      override val use1_at: Int = Pipeline.Stage.E
      override val use2_at: Int = 0
      override val new_at: Int = Pipeline.Stage.M

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := field.rt
      }

      override def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew, mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        m_new.data := use1.data | (B"16'b00" ## field.imm16).asUInt
      }
    }, // ori
    new Inst {
      override val opcode: MaskedLiteral = M"001111--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lui
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = Pipeline.Stage.E

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := field.rt
        d_new.data := (field.imm16 ## B"16'b0").asUInt
      }
    }, // lui
    new Inst {
      override val opcode: MaskedLiteral = M"100011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lw
      override val use1_at: Int = Pipeline.Stage.E
      override val use2_at: Int = 0
      override val new_at: Int = Pipeline.Stage.W

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := field.rt
      }

      override def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew, mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        mem_addr.addr := use1.data + field.imm16.asSInt.resize(Word.width).asUInt
      }

      override def w(inst: Instr, field: Field, use1: RegUse, use2: RegUse, w_new: RegNew, wr: RegNew, mem_addr: MemAddress, mem_store: MemStore, mem_load: MemLoad): Unit = {
        wr.data := mem_load.data
      }
    }, // lw
    new Inst {
      override val opcode: MaskedLiteral = M"101011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sw
      override val use1_at: Int = Pipeline.Stage.E
      override val use2_at: Int = Pipeline.Stage.M
      override val new_at: Int = 0

      override def e(inst: Instr, field: Field, use1: RegUse, use2: RegUse, e_new: RegNew, m_new: RegNew, mdu: MulDivideUnit, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        mem_addr.addr := use1.data + field.imm16.asSInt.resize(Word.width).asUInt
        mem_addr.byteEn := B"4'b1111"
      }

      override def m(inst: Instr, field: Field, use1: RegUse, use2: RegUse, m_new: RegNew, w_new: RegNew, mem_addr: MemAddress, mem_store: MemStore): Unit = {
        mem_store.data := use2.data
      }
    }, // sw
    new Inst {
      override val opcode: MaskedLiteral = M"000100--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.beq
      override val use1_at: Int = Pipeline.Stage.D
      override val use2_at: Int = Pipeline.Stage.D
      override val new_at: Int = 0

      override def br(inst: Instr, field: Field, use1: RegUse, use2: RegUse, ni: NextInstr): Unit = {
        ni.branch := use1.data === use2.data
        ni.pc := inst.addr + 4 + (field.imm16 ## B"2'b00").asSInt.resize(Word.width).asUInt
      }
    }, // beq
    new Inst {
      override val opcode: MaskedLiteral = M"000011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.jal
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = Pipeline.Stage.D

      override def br(inst: Instr, field: Field, use1: RegUse, use2: RegUse, ni: NextInstr): Unit = {
        ni.branch := True
        ni.pc := (inst.addr(31 downto 28) ## field.jump ## B"00").asUInt
      }

      override def d(inst: Instr, field: Field, use1: RegUse, use2: RegUse, d_new: RegNew): Unit = {
        d_new.enable := True
        d_new.addr := 31
        d_new.data := inst.addr + 8
      }
    }, // jal
    new Inst {
      override val opcode: MaskedLiteral = M"000000--------------------001000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.jr
      override val use1_at: Int = Pipeline.Stage.D
      override val use2_at: Int = 0
      override val new_at: Int = 0

      override def br(inst: Instr, field: Field, use1: RegUse, use2: RegUse, ni: NextInstr): Unit = {
        ni.branch := True
        ni.pc := use1.data
      }
    }, // jr
  )

  object Tag extends SpinalEnum {
    val nop: SpinalEnumElement[Tag.this.type] = newElement()
    val add, sub, addu, subu, and, or, xor, nor, slt, sltu, sll, srl, sra, sllv, srlv, srav = newElement()
    val addi, addiu, andi, ori, xori, lui, slti, sltiu = newElement()
    val lw, lh, lhu, lb, lbu, sw, sh, sb = newElement()
    val beq, bne, bgez, bgtz, blez, bltz = newElement()
    val j, jal, jalr, jr = newElement()
    val mult, multu, div, divu, mfhi, mflo, mthi, mtlo = newElement()
  }
}
