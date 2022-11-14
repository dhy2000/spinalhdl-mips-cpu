package datapath

import datapath.Bus._
import spinal.core._

import scala.language.postfixOps

class StallCtrl extends Component {
  // stall at d
  val io = new StallCtrl.IoBundle
  val mdBusy: Bool = io.d_isMd && (io.e_mdBusy)

  val use1: Bool = io.d_use1.use && io.d_use1.addr =/= 0
  val use2: Bool = io.d_use2.use && io.d_use2.addr =/= 0
  val e_wait1: Bool = io.e_new.enable && (io.e_new.addr === io.d_use1.addr || io.e_new.may_new(io.d_use1.addr)) && (io.e_new.new_at.asSInt - io.d_use1.at.asSInt > 1)
  val e_wait2: Bool = io.e_new.enable && (io.e_new.addr === io.d_use2.addr || io.e_new.may_new(io.d_use2.addr)) && (io.e_new.new_at.asSInt - io.d_use2.at.asSInt > 1)
  val m_wait1: Bool = io.m_new.enable && (io.m_new.addr === io.d_use1.addr || io.m_new.may_new(io.d_use1.addr)) && (io.m_new.new_at.asSInt - io.d_use1.at.asSInt > 2)
  val m_wait2: Bool = io.m_new.enable && (io.m_new.addr === io.d_use2.addr || io.m_new.may_new(io.d_use2.addr)) && (io.m_new.new_at.asSInt - io.d_use2.at.asSInt > 2)
  val wait1: Bool = use1 && (e_wait1 || m_wait1)
  val wait2: Bool = use2 && (e_wait2 || m_wait2)

  io.d_stall := (wait1 || wait2) || mdBusy
}

object StallCtrl {
  class IoBundle extends Bundle {
    val d_use1: RegUse = in(new RegUse)
    val d_use2: RegUse = in(new RegUse)
    val e_new: RegNew = in(new RegNew)
    val m_new: RegNew = in(new RegNew)

    // mul/div inst
    val d_isMd: Bool = in Bool()
    val e_mdBusy: Bool = in Bool()

    // output
    val d_stall: Bool = out Bool()
  }
}
