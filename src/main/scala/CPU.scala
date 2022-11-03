import datapath.Pipeline
import global.Const._
import spinal.core._
import spinal.core.fiber.Handle

import scala.language.postfixOps

class CPU extends Component {
  val io = new CPU.IoBundle
  noIoPrefix()
  // synchronous reset
  override val clockDomain: Handle[ClockDomain] = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = ClockDomainConfig(
      resetKind = SYNC
    )
  )
  val pipeline: Pipeline = Pipeline(clockDomain)

  io.macroscopic_pc := 0
  io.i_inst_addr := pipeline.i_inst.addr
  io.m_inst_addr := pipeline.m_inst.addr
  io.m_data_addr := pipeline.m_mem_addr.addr
  io.m_data_wdata := pipeline.m_mem_store.data
  io.m_data_byteen := pipeline.m_mem_addr.byteEn
  io.w_inst_addr := pipeline.w_inst.addr
  io.w_grf_we := pipeline.reg_new.enable
  io.w_grf_addr := pipeline.reg_new.addr
  io.w_grf_wdata := pipeline.reg_new.data
  pipeline.i_inst.code := io.i_inst_rdata
  pipeline.m_mem_load.data := io.m_data_rdata
}

object CPU {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = Generate.target,
    ).generate(new CPU)
  }

  class IoBundle extends Bundle {
    // clock
    val clk: Bool = in Bool()
    val reset: Bool = in Bool()
    // micro sys
//    val interrupt: Bool = in Bool()
    val macroscopic_pc: UInt = out UInt (Word.width bits)
    // Instr Fetch
    val i_inst_rdata: Bits = in Bits  (Word.width bits)
    val i_inst_addr: UInt = out UInt (Word.width bits)
    // Data Memory
    val m_inst_addr: UInt = out UInt (Word.width bits)
    val m_data_addr: UInt = out UInt (Word.width bits)
    val m_data_wdata: UInt = out UInt (Word.width bits)
    val m_data_byteen: Bits = out Bits (Word.byteCount bits)
    val m_data_rdata: UInt = in UInt (Word.width bits)
    // RegisterFile Write
    val w_inst_addr: UInt = out UInt (Word.width bits)
    val w_grf_we: Bool = out Bool()
    val w_grf_addr: UInt = out UInt (Register.addrWidth bits)
    val w_grf_wdata: UInt = out UInt (Word.width bits)
  }
}