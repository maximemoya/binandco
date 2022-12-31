package fr.maxime.binandco
package tools

object Sha256 {

  def getFormattedBlocks(text: String): Array[Array[Byte]] = {

    val textBytes = text.getBytes
    val textLength = textBytes.length

    val extraByte = 0x80.toByte

    val textLengthBitsLong = textLength.toLong * 8L

    val lengthTempBytes: Array[Byte] = BigInt(textLengthBitsLong).toByteArray
    val lengthBytes: Array[Byte] = new Array[Byte](8)
    for (indexToInvert <- lengthTempBytes.indices) {
      lengthBytes.update(7 - indexToInvert, lengthTempBytes.apply(lengthTempBytes.length - 1 - indexToInvert))
    }

    val totalLengthWithout0 = textLength + 1 + 8

    val blockSize = 64
    val blockNumber = ((totalLengthWithout0 - 1) / blockSize) + 1

    val totalLength = blockSize * blockNumber

    val blocks: Array[Array[Byte]] = new Array[Array[Byte]](blockNumber)

    for (blockIndex <- blocks.indices) {
      val bytes = new Array[Byte](blockSize)
      val n = blockIndex * blockSize
      for (indexLocal <- 0 until blockSize) {
        val indexGlobal = indexLocal + n
        if (indexGlobal < textLength) {
          bytes.update(indexLocal, textBytes.apply(indexGlobal))
        }
        else if (indexGlobal == textLength) {
          bytes.update(indexLocal, extraByte)
        }
        else if (indexGlobal >= (totalLength - 8)) {
          bytes.update(indexLocal, lengthBytes.apply(indexLocal % 8))
        }
      }
      blocks.update(blockIndex, bytes)
    }

//    println("Info about Block-Formatted:")
//    val rest0Number = blockSize - (totalLengthWithout0 % blockSize)
//    println(s" > blocks: $blockNumber * $blockSize = ${blockNumber * blockSize} Bytes")
//    println(s" > left 0: $rest0Number Bytes")

    blocks
  }

}
