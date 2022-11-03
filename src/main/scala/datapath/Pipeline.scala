package datapath

import component._
import datapath.Data._
import datapath.Pipeline.forwarded
import instruction.Inst
import spinal.core._

import scala.language.postfixOps

case class Pipeline(override val clockDomain: ClockDomain) extends ClockingArea(clockDomain = clockDomain) {
  private val stall = Bool()
  // F
  private val pc = new ProgramCounter
  val i_inst = new Instr
  private val next_inst = new NextInstr
  // D
  private val d_inst_addr = RegNextWhen(i_inst.addr, !stall) init 0 // pipeline registers
  private val d_inst_code = RegNextWhen(i_inst.code, !stall) init 0 // pipeline registers
  private val d_inst = new Instr
  private val dec = new Decoder
  private val d_field = new Field
  private val grf = new RegisterFile
  private val d_use1 = new RegUse
  private val d_use2 = new RegUse
  private val d_use1_fwd = new RegUse
  private val d_use2_fwd = new RegUse
  private val d_new = new RegNew
  // E
  private val e_inst = RegNext(Mux(stall, Instr(), d_inst)) init Instr()
  private val e_field = RegNext(Mux(stall, Field(), d_field)) init Field()
  private val e_use1 = RegNext(Mux(stall, RegUse(), d_use1_fwd)) init RegUse()
  private val e_use2 = RegNext(Mux(stall, RegUse(), d_use2_fwd)) init RegUse()
  private val e_use1_fwd = new RegUse
  private val e_use2_fwd = new RegUse
  private val e_new = RegNext(Mux(stall, RegNew(), d_new)) init RegNew()
  private val mdu = new MulDivideUnit

  // M
  val m_inst: Instr = RegNext(e_inst) init Instr()
  private val m_field = RegNext(e_field) init Field()
  private val m_use1 = RegNext(e_use1_fwd) init RegUse()
  private val m_use2 = RegNext(e_use2_fwd) init RegUse()
  private val m_use1_fwd = new RegUse
  private val m_use2_fwd = new RegUse
  private val m_new = Reg(new RegNew) init RegNew()
  val m_mem_addr: MemAddress = Reg(new MemAddress) init MemAddress()
  val m_mem_load = new MemLoad
  val m_mem_store: MemStore = new MemStore

  // W
  val w_inst: Instr = RegNext(m_inst) init Instr()
  private val w_field = RegNext(m_field) init Field()
  private val w_use1 = RegNext(m_use1_fwd) init RegUse()
  private val w_use2 = RegNext(m_use2_fwd) init RegUse()
  private val w_new = Reg(new RegNew) init RegNew()
  private val w_mem_addr = RegNext(m_mem_addr) init MemAddress()
  private val w_mem_load = RegNext(m_mem_load) init MemLoad()
  private val w_mem_store = RegNext(m_mem_store) init MemStore()

  val reg_new = new RegNew

  // F
  pc.io.enable := !stall
  pc.io.branch := next_inst.branch
  pc.io.next := next_inst.pc
  i_inst.addr := pc.io.current
  i_inst.inst := Inst.Tag.nop

  // D
  d_inst.addr := d_inst_addr
  d_inst.code := d_inst_code
  d_inst.inst := dec.io.inst

  dec.io.code := d_inst.code
  d_field := dec.io.field
  grf.io.readAddr1 := dec.io.field.rs
  grf.io.readAddr2 := dec.io.field.rt

  d_use1 := RegUse(dec.io.field.rs, grf.io.readData1)
  d_use2 := RegUse(dec.io.field.rt, grf.io.readData2)
  d_use1_fwd := forwarded(d_use1, e_new, m_new, reg_new)
  d_use2_fwd := forwarded(d_use2, e_new, m_new, reg_new)
  d_new := RegNew()
  next_inst := NextInstr()

  // initialize use and new
  // br and d behavior
  Inst.instrSet.foreach(inst => when(d_inst.inst === inst.tag) {
    d_use1.use := Bool(inst.use1_at != 0)
    d_use2.use := Bool(inst.use2_at != 0)
    d_use1.at := U(inst.use1_at)
    d_use2.at := U(inst.use2_at)
    d_new.new_at := U(inst.new_at)
    inst.br(d_inst, d_field, d_use1_fwd, d_use2_fwd, next_inst)
    inst.d(d_inst, d_field, d_use1_fwd, d_use2_fwd, d_new)
  })

  // E
  e_use1_fwd := forwarded(e_use1, m_new, reg_new)
  e_use2_fwd := forwarded(e_use2, m_new, reg_new)
  mdu.io.input.start := False
  mdu.io.input.hi := 0
  mdu.io.input.lo := 0
  mdu.io.input.delay := 0
  m_new := e_new
  m_mem_addr := MemAddress()

  // e behavior
  Inst.instrSet.foreach(inst => when(e_inst.inst === inst.tag) {
    inst.e(e_inst, e_field, e_use1_fwd, e_use2_fwd, e_new, m_new, mdu, m_mem_addr, m_mem_store)
  })

  // M
  m_mem_store := MemStore()
  m_use1_fwd := forwarded(m_use1, reg_new)
  m_use2_fwd := forwarded(m_use2, reg_new)
  w_new := m_new

  // m behavior
  Inst.instrSet.foreach(inst => when(m_inst.inst === inst.tag) {
    inst.m(m_inst, m_field, m_use1_fwd, m_use2_fwd, m_new, w_new, m_mem_addr, m_mem_store)
  })

  // W
  reg_new := w_new

  // w behavior
  Inst.instrSet.foreach(inst => when(w_inst.inst === inst.tag) {
    inst.w(w_inst, w_field, w_use1, w_use2, w_new, reg_new, w_mem_addr, w_mem_store, w_mem_load)
  })

  grf.io.writeEnable := reg_new.enable
  grf.io.writeAddr := reg_new.addr
  grf.io.writeData := reg_new.data

  private val stallUnit = new StallUnit
  stall := stallUnit.io.d_stall
  stallUnit.io.d_use1 := d_use1
  stallUnit.io.d_use2 := d_use2
  stallUnit.io.e_new := e_new
  stallUnit.io.m_new := m_new
  stallUnit.io.d_isMd := B(List(Inst.Tag.mult, Inst.Tag.div, Inst.Tag.multu, Inst.Tag.divu, Inst.Tag.mthi, Inst.Tag.mtlo, Inst.Tag.mfhi, Inst.Tag.mflo)
    .map(d_inst.inst === _)).orR
  stallUnit.io.e_mdCount := mdu.io.status.count
}

object Pipeline {
  def forwarded(use: RegUse, reg_news: RegNew*): RegUse = {
    val fwd = new RegUse
    fwd.use := use.use
    fwd.addr := use.addr
    fwd.at := use.at
    fwd.data := use.data
    reg_news.reverse.foreach(r_new => when(r_new.enable && use.addr === r_new.addr && use.addr =/= 0) {
      fwd.data := r_new.data
    })
    fwd
  }
}
