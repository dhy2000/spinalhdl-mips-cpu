package global

object Const {
  object Word {
    val width = 32
    val byteCount = 4
  }

  object Register {
    val num = 32
    val addrWidth = 5
  }

  object Address {
    val data = 0x0000
    val text = 0x3000
    val exception = 0x4180
  }

  object Pipeline {
    val enable = true
    val numStage = 5
    object Stage {
      val F = 1
      val D = 2
      val E = 3
      val M = 4
      val W = 5
    }
  }

  object Generate {
    val target = "verilog"
  }
}
