package mem

class RAM(size: Int) {
  private val data = Array.fill(size)(0L)

  def read(address: Int): Long = {
    assert(address >= 0 && address < data.length)
    data(address)
  }

  def write(address: Int, byteEnable: Int, writeData: Long): Unit = {
    assert(address >= 0 && address < data.length)
    val dataBytes = for (i <- 0 to 3) yield (data(address) >> (i * 8)) & 0xff
    val writeBytes = for (i <- 0 to 3) yield (writeData >> (i * 8)) & 0xff
    val dataNew = for (i <- 0 to 3) yield if ((byteEnable & (1 << i)) != 0) writeBytes(i) else dataBytes(i)
    data(address) = (for (i <- 0 to 3) yield dataNew(i) << (i * 8)).reduce(_ | _)
  }
}
