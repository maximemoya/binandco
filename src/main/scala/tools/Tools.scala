package fr.maxime.binandco
package tools

import java.nio.charset.Charset
import scala.util.control.Breaks.{break, breakable}

object Tools {

  def printBinariesInfoAboutTextInUTF8(text: String): Unit = {
    println("encode in UTF-8")
    for (i_char <- text.indices) {
      val bytes = text.apply(i_char).toString.getBytes(Charset.forName("UTF-8"))
      val hexStr = bytes.map { byte => String.format("%02X", byte) }.mkString(" + 0x")
      val binStr = bytes.map { byte =>
        String.format("%8s", (byte & 0xFF).toBinaryString).replace(" ", "0")
      }.mkString(" + 0x")
      println(s"'${text.apply(i_char)}' = 0x$hexStr = 0b$binStr")
    }
  }

  def printBinariesInfoAboutInt(i: Int, name: String = ""): Unit = {
    val hexStr = String.format("%02X", i)
    val binStr = String.format("%32s", (i & 0xFFFFFFFF).toBinaryString).replace(" ", "0")
    println(s"integer $i : $name")
    println(s"0x$hexStr\n0b$binStr")
  }

  def printN1ByteArray(array: Array[Byte]): Unit = {
    println(s"[ ${array.map { byte => String.format("%02x", byte) }.mkString("-")} ]")
  }

  def printN1ByteArray(array: Array[Byte], name: String): Unit = {
    println(s" $name:\n[ ${array.map { byte => String.format("%02x", byte) }.mkString("-")} ]")
  }

  def printN2ByteArray(sqrtArray: Array[Array[Byte]]): Unit = {
    println("\nSquareArray info :\n[")
    for (bytes <- sqrtArray) {
      println(s"[ ${bytes.map { element => String.format("%02x", element) }.mkString("-")} ],")
    }
    println("]")
  }

  def printN2IntArray(sqrtArray: Array[Array[Int]]): Unit = {
    println("\nSquareArray info :\n[")
    for (integers <- sqrtArray) {
      println(s"[ ${integers.map { element => String.format("%08x", element) }.mkString("-")} ],")
    }
    println("]")
  }

  // ----------
  // Personal:
  // --------

  private def getOrganizedTextIn64BytesBlocks(text: String): Array[Array[Byte]] = {

    val blockSize = 64

    val bytes = text.getBytes(Charset.forName("UTF-8"))

    val blocksLength = ((bytes.length - 1) / blockSize) + 1

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

    //    println(s"text: $text")
    //    println(s"text length        : ${bytes.length} bytes")
    //    println(s"blocks size        : $blocksLength * $blockSize = ${blocksLength * blockSize} bytes")
    //    val blocksRest = (blocksLength * blockSize) - bytes.length
    //    println(s"blocks rest with 0 : ${blocksLength * blockSize} - ${bytes.length} = $blocksRest bytes")
    //    printN2ByteArray(blocks)

    blocks
  }

  private def getOrganizedTextIn64BytesBlocksWithExtra8Bytes(text: String): Array[Array[Byte]] = {

    val blockSize = 64
    val extraBytesLength = 8

    val textArrayBytes = text.getBytes(Charset.forName("UTF-8"))
    val bytes = new Array[Byte](textArrayBytes.length + extraBytesLength)
    for (i_textByte <- textArrayBytes.indices) {
      bytes.update(i_textByte, textArrayBytes.apply(i_textByte))
    }

    val blocksLength = ((bytes.length - 1) / blockSize) + 1

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

    val lastBlock = blocks.apply(blocks.length - 1)
    for (i_endByte <- 0 until extraBytesLength) {
      val index = blockSize - extraBytesLength + i_endByte
      lastBlock.update(index, 0x11)
      // todo update with real length in hex instead of 0x11
    }

    println(s"text: $text")
    println(s"text length         : ${text.length} characters")
    println(s"textBytes           : ${textArrayBytes.length} bytes")
    println(s"textWithLengthBytes : ${textArrayBytes.length} + $extraBytesLength = ${bytes.length} bytes")
    println(s"blocks size         : $blocksLength * $blockSize = ${blocksLength * blockSize} bytes")
    val blocksRest = (blocksLength * blockSize) - bytes.length
    println(s"blocks rest with 0  : ${blocksLength * blockSize} - ${bytes.length} = $blocksRest bytes")
    printN2ByteArray(blocks)

    blocks
  }

  // --------------
  // EXPERIMENTAL:
  // ------------

  private def organizeTextIn64BytesBlocksWithLengthEndLine(text: String): Unit = {

    val blockSize = 64
    val endLineLength = 8
    val textBytes = text.getBytes(Charset.forName("UTF-8"))

    // ok for textBytes.length from 0 to 256 but need to continue for > 256
    val textBytesHexLengthBytes = new Array[Byte](endLineLength)
    val textBytesLengthHexStr = textBytes.length.toHexString
    println(s"textBytesLengthHexStr = 0x$textBytesLengthHexStr")

    /*
    val textBytesHexLengthBytes = new Array[Byte]((textBytesLengthHexStr.length - 1) / 2 + 1)
    for (i <- textBytesLengthHexStr.indices) {
      if (textBytesLengthHexStr.length % 2 == 0) {
        println("pair")
        if (i < textBytesLengthHexStr.length - 1) {
          val hexStr = s"${textBytesLengthHexStr.apply(i * 2)}${textBytesLengthHexStr.apply((i * 2) + 1)}"
          val byte = Integer.parseInt(hexStr, 16).toByte
          println(s"byte = $byte")
          textBytesHexLengthBytes.update(i, byte)
        }
      }
      else {
        println("impair")
        if (i < 1) {
          val byte = Integer.parseInt(textBytesLengthHexStr.apply(i * 2).toString, 16).toByte
          textBytesHexLengthBytes.update(i, byte)
        }
        else if (i < textBytesLengthHexStr.length - 2) {
          val hexStr = s"${textBytesLengthHexStr.apply(i * 2)}${textBytesLengthHexStr.apply((i * 2) + 1)}"
          val byte = Integer.parseInt(hexStr, 16).toByte
          println(s"byte = $byte")
          textBytesHexLengthBytes.update(i, byte)
        }
      }
    }
    */

    //  val textBytesHexLengthByte = textBytes.length.toByte
    //  val textBytesHexLengthBytes = new Array[Byte](1)
    //  textBytesHexLengthBytes.update(0, textBytesHexLengthByte)

    val bytesLength = textBytes.length + endLineLength
    val bytes: Array[Byte] = new Array(bytesLength)
    breakable {
      for (i <- bytes.indices) {
        if (i < textBytes.length) bytes.update(i, textBytes.apply(i))
        else break
      }
    }

    //  for (i <- textBytesHexLengthBytes.indices) {
    //    bytes.update(bytesLength - 1 - i, textBytesHexLengthBytes.apply(textBytesHexLengthBytes.length - 1 - i))
    //  }

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

    var k = 0
    for (i <- 0 until blocksLength) {
      val block: Array[Byte] = new Array(blockSize)
      for (j <- block.indices) {
        if (((i * blockSize) + j) < bytes.length) {
          block.update(j, bytes.apply(j))
        }
        else if (((i * blockSize) + j) < (blocks.length - endLineLength)) {
          block.update(j, 0x00)
        }
        else {
          k %= textBytesHexLengthBytes.length
          block.update(j, textBytesHexLengthBytes.apply(k))
          k += 1
        }
      }
      blocks.update(i, block)
    }

    printN2ByteArray(blocks)

  }

  private def organizeTextIn256BytesBlocksWithLengthEndLine(text: String): Unit = {

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

}
