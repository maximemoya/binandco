package fr.maxime.binandco
package tools.aes.utils

import tools.aes.interfaces.Table16x16
import tools.aes.interfaces.Table16x16.transformByteAccordingTable16x16

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

  // -------------
  // addRoundKey:
  // -----------

  def addRoundKey(keyExpansion: Array[Array[Int]], round: Int): Unit = {
    val keyArray = keyExpansion(round)
    for (i <- bytes128bits.indices) {
      println(s"bytes128 = ${bytes128bits(i).toInt.toHexString} | key = ${intToByte(keyArray(i / 4), i % 4).toInt.toHexString}")
      val value = (bytes128bits(i) ^ intToByte(keyArray(i / 4), i % 4)).toByte
      bytes128bits.update(i, value)
    }
  }

  // ----------
  // SubBytes:
  // --------

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
   *      SubByte 0x'04' by Table16x16 => 0x'00'
   *      SubByte 0x'd6' by Table16x16 => 0x'01'
   *      SubByte 0x'f1' by Table16x16 => 0x'10'
   *      SubByte 0x'05' by Table16x16 => 0x'22'
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
   *    decoded =>
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

  /**
   * {{{
   *   polynomial calculation of Bytes modulo 0x011b
   *
   *      BytesA: 0b11111111
   *        => X^7 + X^6 + X^5 + X^4 + X^3 + X^2 + X + 1
   *      BytesB: 0b00000110
   *        => X^2 + X
   *
   *      A.B =
   *          ( X^7 + X^6 + X^5 + X^4 + X^3 + X^2 + X + 1 ) * X^2
   *        + ( X^7 + X^6 + X^5 + X^4 + X^3 + X^2 + X + 1 ) * X
   *        = X^9 + 2X^8 + 2X^7 + 2X^6 + 2X^5 + 2X^4 + 2X^3 + 2X^2 + X
   *      factors modulo 2 (1 or 0)
   *        = X^9 + X
   *      equation modulo 0x011b <=> X^8 + X^4 + X^3 + X + 1
   *        = X^9 + X - ( X^8 + X^4 + X^3 + X + 1 ) * X
   *        = X^9 + X - ( X^9 + X^5 + X^4 + X^2 + X )
   *        = - X^5 - X^4 - X^2
   *      absolute factors
   *        = X^5 + X^4 + X^2
   *
   *      A.B =
   *          0b11111111 << 2
   *      XOR 0b11111111 << 1
   *        =
   *          0b1111111100
   *      XOR 0b0111111110
   *        = 0b1000000010
   *    equation modulo 0x011b <=> 0b100011011
   *        = 0b1000000010
   *      XOR 0b1000110110 (0b100011011 << 1)
   *        = 0b0000110100
   *
   *      A.B = X^5 + X^4 + X^2 = 0b110100
   *
   * }}}
   *
   * @param b1 A Byte (8bits)
   * @param b2 A Byte (8bits)
   * @return A Byte (8bits)
   */
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

  /**
   * sign " . " is a [[polynomialMultiplication polynomialMultiplication]]
   * {{{
   *
   *     galoisFieldBox . bytes =
   *
   *       02 03 01 01     00 01 02 03
   *       01 02 03 01     04 05 06 07
   *       01 01 02 03     08 09 0a 0b
   *       03 01 01 02  .  0c 0d 0e 0f
   *
   *       cell calculation: ( MatrixMultiplication : line x column )
   *
   *        for Bytes(0) actual 0x00
   *
   *        new Bytes(0) = ( 0x02.0x00 ) ^ ( 0x03.0x04 ) ^ ( 0x01.0x08 ) ^ ( 0x01.0x0c )
   *                     =      0x00     ^      0x0c     ^      0x08     ^     0x0c
   *                 <=>
   *                     0b00000000
   *                 XOR 0b00001100
   *                 XOR 0b00001000
   *                 XOR 0b00001100
   *                   = 0b00001000
   *                   = 0x08
   *
   *        for Bytes(1) actual 0x01
   *
   *        new Bytes(1) = ( 0x02.0x01 ) ^ ( 0x03.0x05 ) ^ ( 0x01.0x09 ) ^ ( 0x01.0x0d )
   *                     =      0x02     ^      0x0f     ^      0x09     ^     0x0d
   *                 <=>
   *                     0b00000010
   *                 XOR 0b00001111
   *                 XOR 0b00001001
   *                 XOR 0b00001101
   *                   = 0b00001001
   *                   = 0x09
   *
   *        for Bytes(10) actual 0x0a
   *
   *        new Bytes(10) = ( 0x01.0x02 ) ^ ( 0x01.0x06 ) ^ ( 0x02.0x0a ) ^ ( 0x03.0x0e )
   *                     =      0x02     ^      0x06     ^      0x14     ^     0x12
   *                 <=>
   *                     0b00000010
   *                 XOR 0b00000110
   *                 XOR 0b00010100
   *                 XOR 0b00010010
   *                   = 0b00000010
   *                   = 0x02
   *
   *     newBytes =
   *
   *       08 09 0a 0b
   *       1c 1d 1e 1f
   *       00 01 02 03
   *       14 15 16 17
   *
   * }}}
   *
   * @param bytes          An Array of 16 Bytes (128bits)
   * @param galoisFieldBox An Array of 16 Bytes (128bits)
   * @param index          An Integer (32bits)
   * @return A Byte (8bits)
   */
  private def polynomialMatrix(bytes: Array[Byte], galoisFieldBox: Array[Byte], index: Int): Byte = {
    val lineA = index / 4
    val columnA = index % 4
    var byteA: Int = 0x00
    for (i <- 0 until 4) {
      //      println(s"galoisFieldBox ^ byte = ${galoisFieldBox(lineA * 4 + i).toInt.toHexString} ^ ${bytes(i * 4 + columnA).toInt.toHexString}")
      byteA = byteA ^ polynomialMultiplication(galoisFieldBox(lineA * 4 + i), bytes(i * 4 + columnA))
    }
    byteA.toByte
  }

  /**
   * - step1 : polynomial calculation of Bytes modulo 0x011b
   *   - See also [[polynomialMultiplication polynomialMultiplication_method]].
   *
   * - step2 : polynomial calculation of MatrixBytes of 16 Bytes (128bits)
   *   - See also [[polynomialMatrix polynomialMatrix_method]]
   *
   * @param galoisFieldBox An Array of 16 Bytes (128bits)
   */
  def mixColumns(galoisFieldBox: Array[Byte]): Unit = {

    //    println("mixColumns")
    //    println(bytes128bits.map(i => i.toInt.toHexString).mkString(" "))
    //    this.printString()

    val bytesCopy = new Array[Byte](16)
    for (i <- this.bytes128bits.indices) {
      bytesCopy.update(i, this.bytes128bits(i))
    }

    for (i <- this.bytes128bits.indices) {
      this.bytes128bits.update(i, polynomialMatrix(bytesCopy, galoisFieldBox, i))
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

  def reverseBytes128(): Unit = {
    val copyBytes = new Array[Byte](16)
    for (i <- this.bytes128bits.indices) {
      copyBytes.update(i, this.bytes128bits(i))
    }
    this.bytes128bits.update(0, copyBytes(0))
    this.bytes128bits.update(1, copyBytes(4))
    this.bytes128bits.update(2, copyBytes(8))
    this.bytes128bits.update(3, copyBytes(12))

    this.bytes128bits.update(4, copyBytes(1))
    this.bytes128bits.update(5, copyBytes(5))
    this.bytes128bits.update(6, copyBytes(9))
    this.bytes128bits.update(7, copyBytes(13))

    this.bytes128bits.update(8, copyBytes(2))
    this.bytes128bits.update(9, copyBytes(6))
    this.bytes128bits.update(10, copyBytes(10))
    this.bytes128bits.update(11, copyBytes(14))

    this.bytes128bits.update(12, copyBytes(3))
    this.bytes128bits.update(13, copyBytes(7))
    this.bytes128bits.update(14, copyBytes(11))
    this.bytes128bits.update(15, copyBytes(15))
  }

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

  private def setBytes128(bytes: Array[Byte]) = {
    if (bytes.length == 16) {
      bytes
    }
    else {
      throw Error("Bytes128bits().setBytes128(bytes) => bytes.length > 16")
    }
  }

}

object Bytes128bits {

  def of(array16: Array[Byte]): Bytes128bits = {
    Bytes128bits(array16)
  }

  def of(text: String): Bytes128bits = {
    if (text != null) {
      val inputBytes = text.getBytes
      val bytes = new Array[Byte](16)
      bytes.update(0, inputBytes(0))
      bytes.update(1, inputBytes(4))
      bytes.update(2, inputBytes(8))
      bytes.update(3, inputBytes(12))

      bytes.update(4, inputBytes(1))
      bytes.update(5, inputBytes(5))
      bytes.update(6, inputBytes(9))
      bytes.update(7, inputBytes(13))

      bytes.update(8, inputBytes(2))
      bytes.update(9, inputBytes(6))
      bytes.update(10, inputBytes(10))
      bytes.update(11, inputBytes(14))

      bytes.update(12, inputBytes(3))
      bytes.update(13, inputBytes(7))
      bytes.update(14, inputBytes(11))
      bytes.update(15, inputBytes(15))
      Bytes128bits(bytes)
    }
    else {
      throw Error("Bytes128bits(text) => text = null")
    }
  }

  val galoisFieldEncodeBox: Array[Byte] =
    Array(
      0x02.toByte, 0x03.toByte, 0x01.toByte, 0x01.toByte,
      0x01.toByte, 0x02.toByte, 0x03.toByte, 0x01.toByte,
      0x01.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte,
      0x03.toByte, 0x01.toByte, 0x01.toByte, 0x02.toByte
    )

  val galoisFieldDecodeBox: Array[Byte] =
    Array(
      0x0e.toByte, 0x0b.toByte, 0x0d.toByte, 0x09.toByte,
      0x09.toByte, 0x0e.toByte, 0x0b.toByte, 0x0d.toByte,
      0x0d.toByte, 0x09.toByte, 0x0e.toByte, 0x0b.toByte,
      0x0b.toByte, 0x0d.toByte, 0x09.toByte, 0x0e.toByte
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
private class Bytes128bitsBlocks(bytesInput: Array[Byte], fromString: Boolean = false) {

  def apply(): Array[Bytes128bits] = bytesN2

  def apply(blockIndex: Int): Bytes128bits = bytesN2(blockIndex)

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
      val newBytes = Bytes128bits.of(bytes)

      if (fromString) newBytes.reverseBytes128()

      bytesBlocks.update(blockIndex, newBytes)
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
    new Bytes128bitsBlocks(textUTF8.getBytes(Charset.forName("UTF-8")), true)
  }

  def of(binaries: Array[Byte]): Bytes128bitsBlocks = {
    new Bytes128bitsBlocks(binaries)
  }

}
