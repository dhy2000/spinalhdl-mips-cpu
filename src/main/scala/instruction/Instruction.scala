package instruction

import component._
import datapath.Bus._
import global.Const._
import spinal.core._

import scala.language.postfixOps

abstract class Instruction {
  val format: MaskedLiteral
  val tag: SpinalEnumElement[Instruction.Tag.type]

  val use1_at: Int // 0 means not use
  val use2_at: Int // 0 means not use
  val new_at: Int // 0 means not new

  // stages
  def d(bus: Instruction.BusInst, ni: NextInstr): Unit = {}

  def e(bus: Instruction.BusInst, md: MulDivSlot.IoBundle, mem: Instruction.BusMem): Unit = {}

  def m(bus: Instruction.BusInst, mem: Instruction.BusMem): Unit = {}

  def w(bus: Instruction.BusInst, mem: Instruction.BusMem): Unit = {}
}

object Instruction {
  class BusInst(val inst: Instr, val field: Field, val use1: RegUse, val use2: RegUse, val prv_new: RegNew, val nxt_new: RegNew) {}
  class BusMem(val addr: MemAddress, val store: MemStore, val load: MemLoad) {}

  val instrSet: List[Instruction] = List(
    new Instruction {
      override val format: MaskedLiteral = M"00000000000000000000000000000000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.nop
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = 0
    }, // nop
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000100000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.add
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data + bus.use2.data
      }
    }, // add
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000100010"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sub
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data - bus.use2.data
      }
    }, // sub
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000100100"
      override val tag: SpinalEnumElement[Tag.type] = Tag.and
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data & bus.use2.data
      }
    }, // and
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000100101"
      override val tag: SpinalEnumElement[Tag.type] = Tag.or
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data | bus.use2.data
      }
    }, // or
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000101010"
      override val tag: SpinalEnumElement[Tag.type] = Tag.slt
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := (bus.use1.data.asSInt < bus.use2.data.asSInt).asUInt(Word.width bits)
      }
    }, // slt
    new Instruction {
      override val format: MaskedLiteral = M"000000---------------00000101011"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sltu
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.E
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rd
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := (bus.use1.data < bus.use2.data).asUInt(Word.width bits)
      }
    }, // sltu
    new Instruction {
      override val format: MaskedLiteral = M"001111--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lui
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.E

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
        bus.nxt_new.data := (bus.field.imm16 ## B"16'b0").asUInt
      }
    }, // lui
    new Instruction {
      override val format: MaskedLiteral = M"001101--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.ori
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data | (B"16'b00" ## bus.field.imm16).asUInt
      }
    }, // ori
    new Instruction {
      override val format: MaskedLiteral = M"001100--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.andi
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data & (B"16'b00" ## bus.field.imm16).asUInt
      }
    }, // andi
    new Instruction {
      override val format: MaskedLiteral = M"001000--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.addi
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.M

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        bus.nxt_new.data := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
      }
    }, // addi
    new Instruction {
      override val format: MaskedLiteral = M"100011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lw
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.W

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
      }

      override def w(bus: BusInst, mem: BusMem): Unit = {
        bus.nxt_new.data := mem.load.data
      }
    }, // lw
    new Instruction {
      override val format: MaskedLiteral = M"100000--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lb
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.W

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
      }

      override def w(bus: BusInst, mem: BusMem): Unit = {
        bus.nxt_new.data := mem.addr.addr(1 downto 0).muxList(for (i <- 0 until 4) yield (i, mem.load.data(i * 8 - 1 downto i * 8))).asSInt.resize(Word.width).asUInt
      }
    }, // lb
    new Instruction {
      override val format: MaskedLiteral = M"100001--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.lh
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.W

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        bus.nxt_new.enable := True
        bus.nxt_new.addr := bus.field.rt
      }

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
      }

      override def w(bus: BusInst, mem: BusMem): Unit = {
        bus.nxt_new.data := mem.addr.addr(1).asBits.muxList(for (i <- 0 until 2) yield (i, mem.load.data(i * 16 - 1 downto i * 16))).asSInt.resize(Word.width).asUInt
      }
    }, // lh
    new Instruction {
      override val format: MaskedLiteral = M"101011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sw
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.M
      override val new_at: Int = 0

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
        mem.addr.byteEn := B"4'b1111"
      }

      override def m(bus: BusInst, mem: BusMem): Unit = {
        mem.store.data := bus.use2.data
      }
    }, // sw
    new Instruction {
      override val format: MaskedLiteral = M"101000--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sb
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.M
      override val new_at: Int = 0

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
        mem.addr.byteEn := (B"4'b0001" << mem.addr.addr(1 downto 0)).resize(4)
      }

      override def m(bus: BusInst, mem: BusMem): Unit = {
        val by = bus.use2.data(7 downto 0)
        mem.store.data := (by ## by ## by ## by).asUInt
      }
    }, // sb
    new Instruction {
      override val format: MaskedLiteral = M"101001--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.sh
      override val use1_at: Int = PipeStage.E
      override val use2_at: Int = PipeStage.M
      override val new_at: Int = 0

      override def e(bus: BusInst, md: MulDivSlot.IoBundle, mem: BusMem): Unit = {
        mem.addr.addr := bus.use1.data + bus.field.imm16.asSInt.resize(Word.width).asUInt
        mem.addr.byteEn := (B"4'b0011" << (mem.addr.addr(1) ## B"1'b0").asUInt).resize(4)
      }

      override def m(bus: BusInst, mem: BusMem): Unit = {
        val hb = bus.use2.data(15 downto 0)
        mem.store.data := (hb ## hb).asUInt
      }
    }, // sh
    new Instruction {
      override val format: MaskedLiteral = M"000100--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.beq
      override val use1_at: Int = PipeStage.D
      override val use2_at: Int = PipeStage.D
      override val new_at: Int = 0

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        ni.branch := (bus.use1.data === bus.use2.data)
        ni.pc := bus.inst.addr + 4 + (bus.field.imm16 ## B"2'b00").asSInt.resize(Word.width).asUInt
      }
    }, // beq
    new Instruction {
      override val format: MaskedLiteral = M"000101--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.bne
      override val use1_at: Int = PipeStage.D
      override val use2_at: Int = PipeStage.D
      override val new_at: Int = 0

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        ni.branch := (bus.use1.data =/= bus.use2.data)
        ni.pc := bus.inst.addr + 4 + (bus.field.imm16 ## B"2'b00").asSInt.resize(Word.width).asUInt
      }
    }, // bne
    new Instruction {
      override val format: MaskedLiteral = M"000011--------------------------"
      override val tag: SpinalEnumElement[Tag.type] = Tag.jal
      override val use1_at: Int = 0
      override val use2_at: Int = 0
      override val new_at: Int = PipeStage.D

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        ni.branch := True
        ni.pc := (bus.inst.addr(31 downto 28) ## bus.field.jump ## B"00").asUInt
        bus.nxt_new.enable := True
        bus.nxt_new.addr := 31
        bus.nxt_new.data := bus.inst.addr + 8
      }
    }, // jal
    new Instruction {
      override val format: MaskedLiteral = M"000000--------------------001000"
      override val tag: SpinalEnumElement[Tag.type] = Tag.jr
      override val use1_at: Int = PipeStage.D
      override val use2_at: Int = 0
      override val new_at: Int = 0

      override def d(bus: BusInst, ni: NextInstr): Unit = {
        ni.branch := True
        ni.pc := bus.use1.data
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
