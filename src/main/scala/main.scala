package fr.maxime.binandco

import java.nio.charset.Charset

@main
def main(): Unit = {
  val text63 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text64 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text65 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text127 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text128 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text129 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text = text129
  organizeTextIn64BytesBlocks(text)
}

def printBinariesInfoAboutText(text: String): Unit = {

  val bytes = text.getBytes(Charset.forName("UTF-8"))
  for (index <- bytes.indices) {
    println(s"'${text.apply(index)}'= 0x${bytes.apply(index).toInt.toHexString} = 0b${bytes.apply(index).toInt.toBinaryString}")
  }

}

def organizeTextIn64BytesBlocks(text: String): Unit = {

  val blockSize = 64

  val bytes = text.getBytes(Charset.forName("UTF-8"))
  val blocksLength = ((bytes.length - 1) / blockSize) + 1
  val blocksRest = (blocksLength * blockSize) - bytes.length
  println(s"text: $text")
  println(s"text length        : ${bytes.length} bytes")
  println(s"blocks size        : $blocksLength * $blockSize = ${blocksLength * blockSize} bytes")
  println(s"blocks rest with 0 : ${blocksLength * blockSize} - ${bytes.length} = $blocksRest bytes")

  val blocks: Array[Array[Byte]] = new Array(blocksLength)

  for (i <- 0 until blocksLength) {
    val block: Array[Byte] = new Array(blockSize)
    for (j <- block.indices) {
      if (((i * blockSize) + j) < bytes.length) {
        block.update(j, bytes.apply(j))
      }
      else {
        block.update(j, 0x00)
      }
    }
    blocks.update(i, block)
  }

  println("\nBlocks info :\n[")
  for (i <- blocks) {
    println(s"[ ${i.mkString("-")} ],")
  }
  println("]")

}

