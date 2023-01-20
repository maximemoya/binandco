package fr.maxime.binandco
package tools.aes.interfaces.implementations

import tools.aes.interfaces.Table16x16
import tools.aes.interfaces.Table16x16.transformByteAccordingTable16x16
import tools.aes.interfaces.{Bytes128, Bytes128bitsInterface, Table16x16}
import tools.aes.utils.intToByte

object Bytes128bitsImplementationMM {
  def of(s: String): Bytes128bitsInterface =
    bytes128bitsImplementationMM(Bytes128.of(s))

  def of(bytes: Array[Byte]): Bytes128bitsInterface =
    bytes128bitsImplementationMM(Bytes128.of(bytes))

  private val bytes128bitsImplementationMM: Bytes128 => Bytes128bitsInterface = bytes => new Bytes128bitsInterface {
    override val bytes128: Bytes128 = bytes

    override def addRoundKey(bytes: Bytes128, keyExpansion: Array[Array[Int]], round: Int): Bytes128 = {
      val keyArray = keyExpansion(round)
      for (i <- bytes.indices) {
        val value = (bytes(i) ^ intToByte(keyArray(i / 4), i % 4)).toByte
        bytes.update(i, value)
      }
      bytes
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
     *      SubByte 0x'04' by Table16x16 => 0x'00'
     *      SubByte 0x'd6' by Table16x16 => 0x'01'
     *      SubByte 0x'f1' by Table16x16 => 0x'10'
     *      SubByte 0x'05' by Table16x16 => 0x'22'
     *
     * }}}
     *
     * @param bytes       An Array of 16 Bytes (128bits)
     * @param tableEncode A Table16x16 (256Bytes)
     * @return bytes substitutedBytes which is an Array of 16 Bytes (128bits)
     */
    override def subBytes(bytes: Bytes128, tableEncode: Table16x16): Bytes128 = {
      for (index <- bytes.indices) {
        bytes.update(index, transformByteAccordingTable16x16(bytes(index), tableEncode))
      }
      bytes
    }

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
     *
     * @param bytes An Array of 16 Bytes (128bits)
     * @return bytes shiftRowsEncoded which is an Array of 16 Bytes (128bits)
     */
    override def shiftRowsEncode(bytes: Bytes128): Bytes128 = {
      val byteAt4 = bytes(4)
      bytes.update(4, bytes(5))
      bytes.update(5, bytes(6))
      bytes.update(6, bytes(7))
      bytes.update(7, byteAt4)

      val byteAt8 = bytes(8)
      val byteAt9 = bytes(9)
      bytes.update(8, bytes(10))
      bytes.update(9, bytes(11))
      bytes.update(10, byteAt8)
      bytes.update(11, byteAt9)

      val byteAt15 = bytes(15)
      bytes.update(15, bytes(14))
      bytes.update(14, bytes(13))
      bytes.update(13, bytes(12))
      bytes.update(12, byteAt15)
      bytes
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
     *
     * @param bytes An Array of 16 Bytes (128bits)
     * @return bytes shiftRowsDecoded which is an Array of 16 Bytes (128bits)
     */
    override def shiftRowsDecode(bytes: Bytes128): Bytes128 = {

      val byteAt7 = bytes(7)
      bytes.update(7, bytes(6))
      bytes.update(6, bytes(5))
      bytes.update(5, bytes(4))
      bytes.update(4, byteAt7)

      val byteAt8 = bytes(8)
      val byteAt9 = bytes(9)
      bytes.update(8, bytes(10))
      bytes.update(9, bytes(11))
      bytes.update(10, byteAt8)
      bytes.update(11, byteAt9)

      val byteAt12 = bytes(12)
      bytes.update(12, bytes(13))
      bytes.update(13, bytes(14))
      bytes.update(14, bytes(15))
      bytes.update(15, byteAt12)
      bytes

    }

    private def getPolynomial(intUp: Int, strBinDown: String): Byte = {

      val numbs = new Array[Int](strBinDown.count(c => c == '1'))
      var indexNumb = 0
      for (index <- strBinDown.indices) {
        if (strBinDown(index) == '1') {
          val shift = strBinDown.length - 1 - index
          val x = intUp << shift
          numbs.update(indexNumb, x)
          indexNumb += 1
        }
      }

      var intXor = 0
      for (i <- numbs.indices) {
        intXor = intXor ^ numbs(i)
      }

      val moduloIntXor = 0x011b
      while (intXor.toBinaryString.length >= moduloIntXor.toBinaryString.length) {
        val shiftLeft = intXor.toBinaryString.length - moduloIntXor.toBinaryString.length
        val moduloShift = moduloIntXor << shiftLeft
        intXor = intXor ^ moduloShift
      }
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
     * @return polynomialMultiplication of b1.b2 = A Byte (8bits)
     */
    private def polynomialBytesMultiplication(b1: Byte, b2: Byte): Byte = {

      val strBin1: String = (0xff & b1).toBinaryString
      val strBin2: String = (0xff & b2).toBinaryString

      var byteResult: Byte = 0x00
      if ((b1 & 0xff) > (b2 & 0xff))
        byteResult = getPolynomial(b1.toInt & 0xff, strBin2)
      else
        byteResult = getPolynomial(b2.toInt & 0xff, strBin1)
      byteResult
    }

    /**
     * sign " . " is a [[polynomialBytesMultiplication polynomialMultiplication]]
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
     * @param cellIndex      An Integer (32bits)
     * @return polynomialMatrixCellCalculation at cellIndex from bytes = A Byte (8bits)
     */
    private def polynomialMatrixCellCalculation(bytes: Bytes128, galoisFieldBox: Bytes128, cellIndex: Int): Byte = {
      val lineA = cellIndex / 4
      val columnA = cellIndex % 4
      var byteA: Int = 0x00
      for (i <- 0 until 4) {
        byteA = byteA ^ polynomialBytesMultiplication(galoisFieldBox(lineA * 4 + i), bytes(i * 4 + columnA))
      }
      byteA.toByte
    }

    /**
     * - step1 : polynomial calculation of Bytes modulo 0x011b
     *   - See also [[polynomialBytesMultiplication polynomialBytesMultiplication]].
     *
     * - step2 : polynomial calculation of MatrixBytes of 16 Bytes (128bits)
     *   - See also [[polynomialMatrixCellCalculation polynomialMatrixCellCalculation]]
     *
     * @param bytes          An Array of 16 Bytes (128bits)
     * @param galoisFieldBox An Array of 16 Bytes (128bits)
     * @return bytes mixedColumns which is an Array of 16 Bytes (128bits)
     */
    override def mixColumns(bytes: Bytes128, galoisFieldBox: Bytes128): Bytes128 = {

      val bytesCopy = new Array[Byte](16)
      for (i <- bytes.indices) {
        bytesCopy.update(i, bytes(i))
      }
      val bytes128Copy = Bytes128.of(bytesCopy)

      for (i <- bytes.indices) {
        bytes.update(i, polynomialMatrixCellCalculation(bytes128Copy, galoisFieldBox, i))
      }
      bytes

    }

  }

}

object TestIt extends App {

  val bytes128bitsMM: Bytes128bitsInterface = Bytes128bitsImplementationMM.of("abcdefghijklmnop")
  val bytes128 = bytes128bitsMM.bytes128
  println("-initial state:")
  bytes128.printString()

  // ENCODE

  val subEncoded = bytes128bitsMM.subBytes(bytes128, Table16x16.getAesSubstitutionBOX)
  println("-subBytesEncoding...")
  subEncoded.printString()

  val shiftEncoded = bytes128bitsMM.shiftRowsEncode(subEncoded)
  println("-shiftRowsEncoding...")
  shiftEncoded.printString()

  val mixColumnEncoded = bytes128bitsMM.mixColumns(shiftEncoded, Bytes128.galoisFieldEncodeBox)
  println("-mixColumnsEncoding...")
  mixColumnEncoded.printString()

  // DECODE

  val mixColumnDecoded = bytes128bitsMM.mixColumns(mixColumnEncoded, Bytes128.galoisFieldDecodeBox)
  println("-mixColumnDecoding...")
  mixColumnDecoded.printString()

  val shiftDecoded = bytes128bitsMM.shiftRowsDecode(mixColumnDecoded)
  println("-shiftDecoding...")
  shiftDecoded.printString()

  val subDecoded = bytes128bitsMM.subBytes(shiftDecoded, Table16x16.createDecodeTable16x16(Table16x16.getAesSubstitutionBOX))
  println("-subBytesDecoding...")
  subDecoded.printString()

  // idea to encode by chain:
  //  bytes128bitsMM.subBytes(Table16x16.getAesSubstitutionBOX).shiftRowsEncode().mixColumns(Bytes128.galoisFieldEncodeBox)
  // idea to decode by chain:
  //  bytes128bitsMM.mixColumns(Bytes128.galoisFieldDecodeBox).shiftRowsDecode().subBytes(Table16x16.createDecodeTable16x16(Table16x16.getAesSubstitutionBOX))
}
