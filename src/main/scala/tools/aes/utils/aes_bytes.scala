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

  // ---------
  // SubType:
  // -------

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
  // ShiftRow:
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
  def shiftRowEncode(): Unit = {

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
  def shiftRowDecode(): Unit = {

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

  // -----------
  // MixColumn:
  // ---------

  @unused
  def getGaloisFieldEncodeBox: Bytes128bits = {
    new Bytes128bits(
      Array(
        0x02.toByte, 0x03.toByte, 0x01.toByte, 0x01.toByte,
        0x01.toByte, 0x02.toByte, 0x03.toByte, 0x01.toByte,
        0x01.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte,
        0x03.toByte, 0x01.toByte, 0x01.toByte, 0x02.toByte
      )
    )
  }

  @unused
  def getGaloisFieldDecodeBox: Bytes128bits = {
    new Bytes128bits(
      Array(
        0x0e.toByte, 0x0b.toByte, 0x0d.toByte, 0x09.toByte,
        0x09.toByte, 0x0e.toByte, 0x0b.toByte, 0x0d.toByte,
        0x0d.toByte, 0x09.toByte, 0x0e.toByte, 0x0b.toByte,
        0x0b.toByte, 0x0d.toByte, 0x09.toByte, 0x0e.toByte
      )
    )
  }

  
  def mixColumn(galoisFieldBox: Bytes128bits): Bytes128bits = {

    new Bytes128bits(new Array[Byte](16))
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
