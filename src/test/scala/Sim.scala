import mem._
import spinal.core.sim._

import java.io.{File, FileNotFoundException, PrintWriter}
import scala.io.Source

object Sim {
  val codeFile = "code.txt"
  val outputFile = "output.txt"
  val simulateCycles = 10000
  val clockPeriod = 2
  val textStart = 0x3000

  def readCode(): List[Long] = {
    try {
      val file = Source.fromFile(codeFile)
      val code = file.getLines().map(java.lang.Long.parseUnsignedLong(_, 16)).toList
      file.close()
      code
    } catch {
      case _: FileNotFoundException =>
        println(s"$codeFile: no such file!"); List()
    }
  }

  def main(args: Array[String]): Unit = {
    val instMemory = new ROM(4096, readCode())
    val dataMemory = new RAM(4096)
    val outFileWriter = new PrintWriter(new File(outputFile))
    if (args.length < 1) {
      println("no simulation backend given.")
      System.exit(1)
    }
    val backend = args(0)
    (backend.toLowerCase match {
      case "iverilog" => SimConfig.withIVerilog.withVcdWave
      case "verilator" => SimConfig.withVerilator.withVcdWave
      case "vcs" => SimConfig.withVCS.withFsdbWave
      case _ => println(s"invalid simulation backend $backend"); null
    }).compile(new CPU).doSim(dut => {
      // load inst
      forkSensitive(dut.io.i_inst_addr) {
        val addr = ((dut.io.i_inst_addr.toLong - 0x3000) >> 2).toInt
        val inst = instMemory.read(addr)
        dut.io.i_inst_rdata #= inst
      }
      // load mem
      forkSensitive(dut.io.m_data_addr) {
        val addr = (dut.io.m_data_addr.toLong >> 2).toInt
        val data = dataMemory.read(addr)
        dut.io.m_data_rdata #= data
      }

      dut.clockDomain.assertReset()
      dut.clockDomain.forkStimulus(period = clockPeriod)
      SimTimeout(simulateCycles + clockPeriod)
      // reset
      dut.clockDomain.waitFallingEdge()
      dut.clockDomain.deassertReset()

      // load inst
      var time = 0
      while (time < simulateCycles) {
        dut.clockDomain.waitSampling()
        if (dut.io.w_grf_we.toBoolean && dut.io.w_grf_addr.toInt != 0) {
          val registerOut = f"$time%d@${dut.io.w_inst_addr.toLong}%08x: $$${dut.io.w_grf_addr.toInt}%2d <= ${dut.io.w_grf_wdata.toLong}%08x"
          println(registerOut)
          outFileWriter.write(registerOut)
          outFileWriter.write("\n")
          outFileWriter.flush()
        }
        if (dut.io.m_data_byteen.toInt != 0) {
          val addr = (dut.io.m_data_addr.toLong >> 2).toInt
          dataMemory.write(addr, dut.io.m_data_byteen.toInt, dut.io.m_data_wdata.toLong)
          dut.io.m_data_rdata #= dataMemory.read(addr)
          val memoryOut = f"$time%d@${dut.io.m_inst_addr.toLong}%08x: *${dut.io.m_data_addr.toLong}%08x <= ${dataMemory.read(addr)}%08x"
          println(memoryOut)
          outFileWriter.write(memoryOut)
          outFileWriter.write("\n")
          outFileWriter.flush()
        }
        time += clockPeriod
      }
    })
    outFileWriter.close()
  }
}
