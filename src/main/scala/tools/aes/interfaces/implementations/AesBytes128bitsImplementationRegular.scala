package fr.maxime.binandco
package tools.aes.interfaces.implementations

import tools.aes.interfaces.Table16x16.transformByteAccordingTable16x16
import tools.aes.interfaces.{AesBytes128bitsInterface, Bytes128, KeyExpansion128bits, Table16x16}

object AesBytes128bitsImplementationRegular {

  def of(s: String): AesBytes128bitsInterface =
    val bytes128 = Bytes128.of(s)
    bytes128.reverseBytes128()
    aesBytes128bitsImplementationRegular(bytes128)

  def of(bytes: Array[Byte]): AesBytes128bitsInterface =
    aesBytes128bitsImplementationRegular(Bytes128.of(bytes))

  private def aesBytes128bitsImplementationRegular(bytes: Bytes128): AesBytes128bitsInterface =
    new AesBytes128bitsInterface {
      override val bytes128: Bytes128 = bytes

      // AddRoundKey

      override def addRoundKey(keyExpansion: KeyExpansion128bits, round: Int): AesBytes128bitsInterface = {
        val keyArray = keyExpansion(round)
        for (i <- this.bytes128.indices) {
          val value = (this.bytes128(i) ^ KeyExpansion128bits.intToByte(keyArray(i / 4), i % 4)).toByte
          this.bytes128.update(i, value)
        }
        this
      }

      // SubBytes

      /**
       * Convert Byte to Byte this.bytes128bits with Table16x16
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
       * @return this as Bytes128bitsInterface
       */
      override def subBytes(tableEncode: Table16x16): AesBytes128bitsInterface = {
        for (index <- this.bytes128.indices) {
          this.bytes128.update(index, transformByteAccordingTable16x16(this.bytes128(index), tableEncode))
        }
        this
      }

      // ShiftRows

      /**
       * {{{
       *   shift row's of this.bytes128bits :
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
       * @return this as Bytes128bitsInterface
       */
      override def shiftRowsEncode(): AesBytes128bitsInterface = {
        val byteAt4 = this.bytes128(4)
        this.bytes128.update(4, this.bytes128(5))
        this.bytes128.update(5, this.bytes128(6))
        this.bytes128.update(6, this.bytes128(7))
        this.bytes128.update(7, byteAt4)

        val byteAt8 = this.bytes128(8)
        val byteAt9 = this.bytes128(9)
        this.bytes128.update(8, this.bytes128(10))
        this.bytes128.update(9, this.bytes128(11))
        this.bytes128.update(10, byteAt8)
        this.bytes128.update(11, byteAt9)

        val byteAt15 = this.bytes128(15)
        this.bytes128.update(15, this.bytes128(14))
        this.bytes128.update(14, this.bytes128(13))
        this.bytes128.update(13, this.bytes128(12))
        this.bytes128.update(12, byteAt15)
        this
      }

      /**
       * {{{
       *   shift row's of this.bytes128bits :
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
       * @return this as Bytes128bitsInterface
       */
      override def shiftRowsDecode(): AesBytes128bitsInterface = {

        val byteAt7 = this.bytes128(7)
        this.bytes128.update(7, this.bytes128(6))
        this.bytes128.update(6, this.bytes128(5))
        this.bytes128.update(5, this.bytes128(4))
        this.bytes128.update(4, byteAt7)

        val byteAt8 = this.bytes128(8)
        val byteAt9 = this.bytes128(9)
        this.bytes128.update(8, this.bytes128(10))
        this.bytes128.update(9, this.bytes128(11))
        this.bytes128.update(10, byteAt8)
        this.bytes128.update(11, byteAt9)

        val byteAt12 = this.bytes128(12)
        this.bytes128.update(12, this.bytes128(13))
        this.bytes128.update(13, this.bytes128(14))
        this.bytes128.update(14, this.bytes128(15))
        this.bytes128.update(15, byteAt12)
        this

      }

      // MixColumns

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
       * @param galoisFieldBox An Array of 16 Bytes (128bits)
       * @return this as Bytes128bitsInterface
       */
      override def mixColumns(galoisFieldBox: Bytes128): AesBytes128bitsInterface = {

        val bytesCopy = new Array[Byte](16)
        for (i <- this.bytes128.indices) {
          bytesCopy.update(i, this.bytes128(i))
        }
        val bytes128Copy = Bytes128.of(bytesCopy)

        for (i <- this.bytes128.indices) {
          this.bytes128.update(i, polynomialMatrixCellCalculation(bytes128Copy, galoisFieldBox, i))
        }
        this

      }

    }

}

object TestIt extends App {

  private val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of("abcdefghijklmnop")
  println("-initial state:")
  aesBytes128bitsRegular.printBytes()

  // CHAIN METHODS:

  aesBytes128bitsRegular
    .subBytes(Table16x16.getAesSubstitutionBOX)
    .shiftRowsEncode()
    .mixColumns(Bytes128.galoisFieldEncodeBox)
  println("chainEncode:")
  aesBytes128bitsRegular.printBytes()

  aesBytes128bitsRegular
    .mixColumns(Bytes128.galoisFieldDecodeBox)
    .shiftRowsDecode()
    .subBytes(Table16x16.createDecodeTable16x16(Table16x16.getAesSubstitutionBOX))
  println("chainDecode:")
  aesBytes128bitsRegular.printBytes()

}
