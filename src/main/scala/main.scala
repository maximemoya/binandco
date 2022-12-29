package fr.maxime.binandco

import java.nio.charset.Charset
import scala.util.control.Breaks.{break, breakable}

@main
def main(): Unit = {
  val text63 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text64 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text65 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text127 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text128 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text129 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text239 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text240 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text241 = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
  val text = text64

  //  organizeTextIn64BytesBlocks(text)
  organizeTextIn64BytesBlocksWithLengthEndLine(text)

  //  organizeTextIn256BytesBlocksWithLengthEndLine(text)
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

def organizeTextIn64BytesBlocksWithLengthEndLine(text: String): Unit = {

  val blockSize = 64
  val endLineLength = 8
  val textBytes = text.getBytes(Charset.forName("UTF-8"))

  // ok for textBytes.length from 0 to 256 but need to continue for > 256
  val textBytesHexLengthByte = textBytes.length.toByte
  val textBytesHexLengthBytes = new Array[Byte](1)
  textBytesHexLengthBytes.update(0, textBytesHexLengthByte)

  val bytesLength = textBytes.length + endLineLength
  val bytes: Array[Byte] = new Array(bytesLength)
  breakable {
    for (i <- bytes.indices) {
      if (i < textBytes.length) bytes.update(i, textBytes.apply(i))
      else break
    }
  }
  for (i <- textBytesHexLengthBytes.indices) {
    bytes.update(bytesLength - 1 - i, textBytesHexLengthBytes.apply(textBytesHexLengthBytes.length - 1 - i))
  }

  printN1ByteArray(textBytesHexLengthBytes)
  printN1ByteArray(bytes)

  val blocksLength = ((bytes.length - 1) / blockSize) + 1
  val blocksRest = (blocksLength * blockSize) - bytes.length

  println(s"text: $text")
  println(s"textBytes            : ${textBytes.length} bytes")
  println(s"textBytes hex-length : ${textBytesHexLengthBytes.length} bytes")
  println(s"bytes length         : ${textBytes.length} + $endLineLength = ${bytes.length} bytes")
  println(s"blocks size          : $blocksLength * $blockSize = ${blocksLength * blockSize} bytes")
  println(s"blocks rest with 0   : ${blocksLength * blockSize} - ${bytes.length} = $blocksRest bytes")

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

  printN2ByteArray(blocks)
  //  println("\nBlocks info :\n[")
  //  for (i <- blocks) {
  //    println(s"[ ${i.mkString("-")} ],")
  //  }
  //  println("]")

}

def organizeTextIn256BytesBlocksWithLengthEndLine(text: String): Unit = {

  val blockSize = 256
  val endLineLength = 16
  val textBytes = text.getBytes(Charset.forName("UTF-8"))
  val textBytesHexLengthBytes = textBytes.length.toHexString.getBytes(Charset.forName("UTF-8"))

  val bytesLength = textBytes.length + endLineLength
  val bytes: Array[Byte] = new Array(bytesLength)
  for (i <- bytes.indices) {
    if (i < textBytes.length) bytes.update(i, textBytes.apply(i))
    else bytes.update(i, 0x00)
  }

  println(s"[ ${bytes.mkString("-")} ]")

  val blocksLength = ((bytes.length - 1) / blockSize) + 1
  val blocksRest = (blocksLength * blockSize) - bytes.length

  println(s"text: $text")
  println(s"textBytes            : ${textBytes.length} bytes")
  println(s"textBytes hex-length : ${textBytesHexLengthBytes.length} bytes")
  println(s"bytes length         : ${textBytes.length} + $endLineLength = ${bytes.length} bytes")
  println(s"blocks size          : $blocksLength * $blockSize = ${blocksLength * blockSize} bytes")
  println(s"blocks rest with 0   : ${blocksLength * blockSize} - ${bytes.length} = $blocksRest bytes")

  // to continue =>

  val blocks: Array[Array[Byte]] = new Array(blocksLength)

  for (i <- 0 until blocksLength) {
    val block: Array[Byte] = new Array(blockSize)
    for (j <- block.indices) {
      if (((i * blockSize) + j) < bytes.length) block.update(j, bytes.apply(j))
      else block.update(j, 0x00)
    }
    blocks.update(i, block)
  }

  //  printSqrtByteArray(blocks)

}

def printN1ByteArray(array: Array[Byte]): Unit = {
  println(s"[ ${array.map { byte => String.format("%02x", byte) }.mkString("-")} ]")
}

def printN2ByteArray(sqrtArray: Array[Array[Byte]]): Unit = {
  println("\nSquareArray info :\n[")
  for (byte <- sqrtArray) {
    println(s"[ ${byte.map { element => String.format("%02x", element) }.mkString("-")} ],")
  }
  println("]")
}
