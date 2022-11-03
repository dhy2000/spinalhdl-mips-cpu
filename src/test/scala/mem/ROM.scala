package mem

class ROM(size: Int, fill: List[Long]) {
  private val data = Array.fill(size)(0L)
  for (i <- data.indices) {
    if (i < fill.length) {
      data(i) = fill(i)
    }
  }

  def read(address: Int): Long = {
    // addressing by word
    assert(address >= 0 && address < data.length)
    data(address)
  }
}
