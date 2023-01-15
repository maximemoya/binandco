package fr.maxime.binandco
package tools.aes.utils

import tools.aes.Table16x16

import java.nio.charset.Charset
import scala.annotation.unused

/**
 * {{{
 *   Array of 16 Bytes (a block 4*4*8 = 128bits)
 *   Used to proceed bytes block operation (AES...)
 *
 *   example:
 *      bytes128bits:
 *          0x[
 *            6d 6d 6d 6d,
 *            6d 6d 6d 6d,
 *            6d 6d 6d 6d,
 *            6d 6d 6d 6d
 *           ]
 * }}}
 *
 * @param bytes Array of Bytes
 */
private class Bytes128bits(bytes: Array[Byte]) {
  def apply(): Array[Byte] = bytes128bits

  // ----------
  // SubBytes:
  // --------

  private def transformByteAccordingTable16x16(byte: Byte, table: Table16x16): Byte = {

    //    val str = String.format("%02x", byte)
    //    val line = Integer.parseInt(str(0).toString, 16)
    //    val column = Integer.parseInt(str(1).toString, 16)
    //
    //    println(s"byte= $byte | 0x${String.format("%02x", byte)}")
    //    println(s"str= \"$str\"")
    //    println(s"line= ${line + 1}")
    //    println(s"column= ${column + 1}")

    var indexToReach: Int = byte
    if (byte < 0) indexToReach = byte + 256

    //    println(s"arr($indexToReach)=${String.format("%02x", table.get(indexToReach))}")

    table.get(indexToReach)

  }

  /**
   * Convert Byte by Byte the Bytes128bits with Table16x16
   * {{{
   * Example:
   *   part of Table16x16:
   *           0   1   2   ...  15
   *      0x[
   *   0      04, d6, 18, ....
   *   1      f1, 45, 6a, ....
   *   2      5d, b0, 05, ....
   *  ...     ................
   *   15     ................
   *      ]
   *
   *      Bytes128bits = 0x[ 04, d6, f1, 05, ...]
   *
   *      SubType 0x'04' by Table16x16 => 0x'00'
   *      SubType 0x'd6' by Table16x16 => 0x'01'
   *      SubType 0x'f1' by Table16x16 => 0x'10'
   *      SubType 0x'05' by Table16x16 => 0x'22'
   *
   * }}}
   *
   * @param tableEncode A Table16x16 (256Bytes)
   */
  def subBytes(tableEncode: Table16x16): Unit = {
    for (index <- this.bytes128bits.indices) {
      this.bytes128bits.update(index, transformByteAccordingTable16x16(this.bytes128bits(index), tableEncode))
    }
  }

  // ----------
  // ShiftRows:
  // --------

  /**
   * {{{
   *   shift row's of bytes128bits :
   *    from =>
   *      0x[
   *         00,01,02,03,
   *         00,01,02,03, <= rotate left by 1
   *         00,01,02,03, <= rotate left by 2
   *         00,01,02,03  <= rotate left by 3
   *       ]
   *    encoded =>
   *      0x[
   *         00,01,02,03,
   *         01,02,03,00,
   *         02,03,00,01,
   *         03,00,01,02
   *       ]
   * }}}
   */
  def shiftRowsEncode(): Unit = {

    val byteAt4 = this.bytes128bits(4)
    this.bytes128bits.update(4, this.bytes128bits(5))
    this.bytes128bits.update(5, this.bytes128bits(6))
    this.bytes128bits.update(6, this.bytes128bits(7))
    this.bytes128bits.update(7, byteAt4)

    val byteAt8 = this.bytes128bits(8)
    val byteAt9 = this.bytes128bits(9)
    this.bytes128bits.update(8, this.bytes128bits(10))
    this.bytes128bits.update(9, this.bytes128bits(11))
    this.bytes128bits.update(10, byteAt8)
    this.bytes128bits.update(11, byteAt9)

    val byteAt15 = this.bytes128bits(15)
    this.bytes128bits.update(15, this.bytes128bits(14))
    this.bytes128bits.update(14, this.bytes128bits(13))
    this.bytes128bits.update(13, this.bytes128bits(12))
    this.bytes128bits.update(12, byteAt15)

  }

  /**
   * {{{
   *   shift row's of bytes128bits :
   *    from =>
   *      0x[
   *         00,01,02,03,
   *         01,02,03,00, => rotate right by 1
   *         02,03,00,01, => rotate right by 2
   *         03,00,01,02  => rotate right by 3
   *       ]
   *    encoded =>
   *      0x[
   *         00,01,02,03,
   *         00,01,02,03,
   *         00,01,02,03,
   *         00,01,02,03,
   *       ]
   * }}}
   */
  def shiftRowsDecode(): Unit = {

    val byteAt7 = this.bytes128bits(7)
    this.bytes128bits.update(7, this.bytes128bits(6))
    this.bytes128bits.update(6, this.bytes128bits(5))
    this.bytes128bits.update(5, this.bytes128bits(4))
    this.bytes128bits.update(4, byteAt7)

    val byteAt8 = this.bytes128bits(8)
    val byteAt9 = this.bytes128bits(9)
    this.bytes128bits.update(8, this.bytes128bits(10))
    this.bytes128bits.update(9, this.bytes128bits(11))
    this.bytes128bits.update(10, byteAt8)
    this.bytes128bits.update(11, byteAt9)

    val byteAt12 = this.bytes128bits(12)
    this.bytes128bits.update(12, this.bytes128bits(13))
    this.bytes128bits.update(13, this.bytes128bits(14))
    this.bytes128bits.update(14, this.bytes128bits(15))
    this.bytes128bits.update(15, byteAt12)

  }

  // ------------
  // MixColumns:
  // ----------

  private def getPolynomial(intMax: Int, strBinMin: String): Byte = {

    //    println(s"intMax    = ${String.format("%8s", (intMax & 0xff).toBinaryString).replace(' ', '0')}")
    //    println(s"strBinMin = ${String.format("%8s", strBinMin).replace(' ', '0')}")

    val numbs = new Array[Int](strBinMin.count(c => c == '1'))
    var indexNumb = 0

    for (index <- strBinMin.indices) {
      if (strBinMin(index) == '1') {
        val shift = strBinMin.length - 1 - index
        val x = intMax << shift
        numbs.update(indexNumb, x)
        indexNumb += 1

        //        println(s"at index: $index")
        //        println(s"x = ${String.format("%32s", intMax.toBinaryString).replace(' ', '0')} << $shift")
        //        println(s"x = ${String.format("%32s", x.toBinaryString).replace(' ', '0')}")

      }
    }

    //    println("\n\t-------------\n")

    var intXor = 0
    for (i <- numbs.indices) {
      //      println(s"intXor = ${String.format("%32s", intXor.toBinaryString).replace(' ', '0')}" +
      //        s"\n     xor ${String.format("%32s", numbs(i).toBinaryString).replace(' ', '0')}\n")
      intXor = intXor ^ numbs(i)
    }

    //    println(s"intXor = ${String.format("%32s", intXor.toBinaryString).replace(' ', '0')}")
    //    println("\n\t-------------\n")
    //    println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}")

    val moduloIntXor = 0x011b

    //    println(s"moduloIntXor = ${String.format("%32s", moduloIntXor.toBinaryString)}")

    while (intXor.toBinaryString.length >= moduloIntXor.toBinaryString.length) {
      val shiftLeft = intXor.toBinaryString.length - moduloIntXor.toBinaryString.length
      //      println()
      //      println(s"shiftLeft: $shiftLeft")

      val moduloShift = moduloIntXor << shiftLeft

      //      println(s"moduloShift  = ${String.format("%32s", moduloIntXor.toBinaryString)} << $shiftLeft")
      //      println(s"moduloShift  = ${String.format("%32s", moduloShift.toBinaryString)}")
      //      println()
      //      println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}" +
      //        s"\n           xor ${String.format("%32s", moduloShift.toBinaryString)}")

      intXor = intXor ^ moduloShift

      //      println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}")

    }

    //    println("\n\t-------------\n")
    //    println(s"intXor = ${String.format("%10s", intXor.toBinaryString)}")
    //    println(s"intXor = 0b${String.format("%8s", intXor.toBinaryString).replace(' ', '0')} 0x${String.format("%2s", intXor.toHexString).replace(' ', '0')}")

    intXor.toByte
  }

  private def polynomialMultiplication(b1: Byte, b2: Byte): Byte = {

    val strBin1: String = (0xff & b1).toBinaryString
    val strBin2: String = (0xff & b2).toBinaryString

    //    println(s"b1        = ${String.format("%8s", (b1.toInt & 0xff).toBinaryString).replace(' ', '0')}")
    //    println(s"b2        = ${String.format("%8s", (b2.toInt & 0xff).toBinaryString).replace(' ', '0')}")

    //    val b1Bin = s"${String.format("%8s", (b1.toInt & 0xff).toBinaryString).replace(' ', '0')}"
    //    val b2Bin = s"${String.format("%8s", (b2.toInt & 0xff).toBinaryString).replace(' ', '0')}"
    //    val b1Hex = s"${String.format("%2s", (b1.toInt & 0xff).toHexString).replace(' ', '0')}"
    //    val b2Hex = s"${String.format("%2s", (b2.toInt & 0xff).toHexString).replace(' ', '0')}"

    if ((b1 & 0xff) > (b2 & 0xff)) {
      val byteResult = getPolynomial(b1.toInt & 0xff, strBin2)
      //      println(s"\n$b1Bin . $b2Bin = ${String.format("%8s", (byteResult & 0xff).toBinaryString).replace(' ', '0')}")
      //      println(s"$b1Hex . $b2Hex = ${String.format("%2s", (byteResult & 0xff).toHexString).replace(' ', '0')}")
      byteResult
    }
    else {
      val byteResult = getPolynomial(b2.toInt & 0xff, strBin1)
      //      println(s"\n$b1Bin . $b2Bin = ${String.format("%8s", (byteResult & 0xff).toBinaryString).replace(' ', '0')}")
      //      println(s"$b1Hex . $b2Hex = ${String.format("%2s", (byteResult & 0xff).toHexString).replace(' ', '0')}")
      byteResult
    }
  }

  private def polynomialMatrix(bytes: Bytes128bits, galoisFieldBox: Bytes128bits, index: Int): Byte = {
    val lineA = index / 4
    val columnA = index % 4
    var byteA: Int = 0x00
    for (i <- 0 until 4) {
      byteA = byteA ^ polynomialMultiplication(galoisFieldBox.bytes128bits(lineA * 4 + i), bytes.bytes128bits(i * 4 + columnA))
    }
    byteA.toByte
  }

  def mixColumns(galoisFieldBox: Bytes128bits): Unit = {

    val bytesCopy = new Array[Byte](16)
    for (i <- this.bytes128bits.indices) {
      bytesCopy.update(i, this.bytes128bits(i))
    }
    val bytes128bitsCopy = new Bytes128bits(bytesCopy)

    for (i <- bytes128bits.indices) {
      this.bytes128bits.update(i, polynomialMatrix(bytes128bitsCopy, galoisFieldBox, i))
    }

    //    println(s"bytes128bitsCopy:")
    //    bytes128bitsCopy.printString()
    //    println(s"\n\tmixColumn\n")
    //    println(s"bytes128bits:")
    //    this.printString()

  }

  // ---------------
  // Other Methods:
  // -------------

  def printString(): Unit = {
    var str = ""
    for (byteIndex <- this.bytes128bits.indices) {
      if (byteIndex % 4 == 0) {
        str += s"\n\t${String.format("%02x", this.bytes128bits(byteIndex))} "
      }
      else {
        str += s"${String.format("%02x", this.bytes128bits(byteIndex))} "
      }
    }
    str += "\n"
    println(str)
  }

  // ---------------------------------------------
  // PRIVATE PROPERTIES AUTHORIZED PERSONAL ONLY:
  // -------------------------------------------

  private val bytes128bits = setBytes128(bytes)

  private def setBytes128(inputBytes: Array[Byte]) = {
    val bytes = new Array[Byte](16)
    if (inputBytes.length >= 16) {
      for (i <- bytes.indices) {
        bytes.update(i, inputBytes(i))
      }
    }
    else {
      for (i <- inputBytes.indices) {
        bytes.update(i, inputBytes(i))
      }
    }
    bytes
  }
}

object Bytes128bits {

  val galoisFieldEncodeBox: Bytes128bits = new Bytes128bits(
    Array(
      0x02.toByte, 0x03.toByte, 0x01.toByte, 0x01.toByte,
      0x01.toByte, 0x02.toByte, 0x03.toByte, 0x01.toByte,
      0x01.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte,
      0x03.toByte, 0x01.toByte, 0x01.toByte, 0x02.toByte
    )
  )

  val galoisFieldDecodeBox: Bytes128bits = new Bytes128bits(
    Array(
      0x0e.toByte, 0x0b.toByte, 0x0d.toByte, 0x09.toByte,
      0x09.toByte, 0x0e.toByte, 0x0b.toByte, 0x0d.toByte,
      0x0d.toByte, 0x09.toByte, 0x0e.toByte, 0x0b.toByte,
      0x0b.toByte, 0x0d.toByte, 0x09.toByte, 0x0e.toByte
    )
  )

}

/**
 * {{{
 *   Arrays of Array[Byte] formatted by 16Bytes => 128 bits per block
 *   Used to proceed bytes block operation (AES...)
 *
 *   example:
 *      bytesInput = "mm" (0x[6d 6d])
 *        will produce a Bytes128bits => 0x[[6d 6d 00 00 00 00 00 00]]
 *
 *      bytesInput = "mmmmmmmmm" (0x[6d 6d 6d 6d 6d 6d 6d 6d 6d])
 *        will produce a Bytes128bits =>
 *        0x[
 *          [6d 6d 6d 6d 6d 6d 6d 6d],
 *          [6d 00 00 00 00 00 00 00],
 *        ]
 *
 * }}}
 *
 * @param bytesInput Array of Bytes
 */
private class Bytes128bitsBlocks(bytesInput: Array[Byte]) {

  def apply(): Array[Bytes128bits] = bytesN2

  def get(): Array[Bytes128bits] = bytesN2

  def get(blockIndex: Int): Bytes128bits = bytesN2(blockIndex)

  private val bytesN2: Array[Bytes128bits] = setWithInputBytes(bytesInput)

  private def setWithInputBytes(bytesInput: Array[Byte]): Array[Bytes128bits] = {
    val bytesBlocks = new Array[Bytes128bits](((bytesInput.length - 1) / 16) + 1)
    for (blockIndex <- bytesBlocks.indices) {
      val bytes = new Array[Byte](16)
      if (bytesInput.length - (blockIndex * 16) >= 16) {
        for (byteIndex <- 0 until 16) {
          bytes.update(byteIndex, bytesInput(blockIndex * 16 + byteIndex))
        }
      }
      else {
        for (byteIndex <- 0 until (bytesInput.length % 16)) {
          bytes.update(byteIndex, bytesInput(blockIndex * 16 + byteIndex))
        }
      }
      bytesBlocks.update(blockIndex, new Bytes128bits(bytes))
    }
    bytesBlocks
  }

  def printString(): Unit = {
    var str = s"n2Bytes {${this.bytesN2.length}Blocks / ${this.bytesN2.length * 16}Bytes / ${this.bytesN2.length * 128}bits}\n=> in Hexa {0xff}"
    for (blockIndex <- this.bytesN2.indices) {
      for (byteIndex <- this.bytesN2(blockIndex)().indices) {
        if (byteIndex % 4 == 0) {
          str += s"\n\t${String.format("%02x", this.bytesN2(blockIndex)()(byteIndex))} "
        }
        else {
          str += s"${String.format("%02x", this.bytesN2(blockIndex)()(byteIndex))} "
        }
      }
      if (blockIndex < this.bytesN2.length - 1) {
        str += "\n"
      }
    }
    str += "\n"
    println(str)
  }

}

object Bytes128bitsBlocks {
  def of(textUTF8: String): Bytes128bitsBlocks = {
    new Bytes128bitsBlocks(textUTF8.getBytes(Charset.forName("UTF-8")))
  }

  def of(binaries: Array[Byte]): Bytes128bitsBlocks = {
    new Bytes128bitsBlocks(binaries)
  }

}

def getRoundConstants: Array[Int] = {
Array(
    0x01000000,
    0x02000000,
    0x04000000,
    0x08000000,
    0x10000000,
    0x20000000,
    0x40000000,
    0x80000000,
    0x1B000000,
    0x36000000,
    0x6C000000,
    0xD8000000,
    0xAB000000,
    0x4D000000,
    0x9A000000,
  )
}
