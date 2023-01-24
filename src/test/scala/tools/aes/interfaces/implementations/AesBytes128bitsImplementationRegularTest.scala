package fr.maxime.binandco
package tools.aes.interfaces.implementations

import tools.aes.interfaces.{AesBytes128bitsInterface, Bytes128, KeyExpansion128bits, Table16x16}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.nio.charset.StandardCharsets

class AesBytes128bitsImplementationRegularTest extends AnyFlatSpec with should.Matchers {

  // ----------
  // ENCODING:
  // -------

  "SubBytes ENCODING" should "succeed Encoding" in {

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    ))

    val array256 = new Array[Byte](256)
    for (i <- array256.indices) {
      array256.update(i, (255 - i).toByte)
    }
    val table16x16Encode = new Table16x16(array256)

    val bytes128bitsEncoded = Array(
      0x9e.toByte, 0x9d.toByte, 0x9c.toByte, 0x9b.toByte,
      0x9a.toByte, 0x99.toByte, 0x98.toByte, 0x97.toByte,
      0x96.toByte, 0x95.toByte, 0x94.toByte, 0x93.toByte,
      0x92.toByte, 0x91.toByte, 0x90.toByte, 0x8f.toByte
    )

    aesBytes128bitsRegular.subBytes(table16x16Encode)
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsEncoded)

  }

  "ShiftRows ENCODING" should "succeed Encoding" in {

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    ))

    val bytes128bitsEncoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x66.toByte, 0x67.toByte, 0x68.toByte, 0x65.toByte,
      0x6b.toByte, 0x6c.toByte, 0x69.toByte, 0x6a.toByte,
      0x70.toByte, 0x6d.toByte, 0x6e.toByte, 0x6f.toByte
    )

    aesBytes128bitsRegular.shiftRowsEncode()
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsEncoded)

  }

  "mixColumns ENCODING" should "succeed Encoding" in {

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(Array(
      0x63.toByte, 0xeb.toByte, 0x9f.toByte, 0xa0.toByte,
      0x2f.toByte, 0x93.toByte, 0x92.toByte, 0xc0.toByte,
      0xaf.toByte, 0xc7.toByte, 0xab.toByte, 0x30.toByte,
      0xa2.toByte, 0x20.toByte, 0xcb.toByte, 0x2b.toByte
    ))
    val galoisFieldEncode = Bytes128.galoisFieldEncodeBox

    val bytes128bitsEncoded = Array(
      0xba.toByte, 0x84.toByte, 0xe8.toByte, 0x1b.toByte,
      0x75.toByte, 0xa4.toByte, 0x8d.toByte, 0x40.toByte,
      0xf4.toByte, 0x8d.toByte, 0x06.toByte, 0x7d.toByte,
      0x7a.toByte, 0x32.toByte, 0x0e.toByte, 0x5d.toByte
    )

    aesBytes128bitsRegular.mixColumns(galoisFieldEncode)
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsEncoded)
  }

  // -----------------
  // 1stRound Encode:
  // ---------------

  "1stRound ENCODING" should "succeed encoding" in {

    val keyBytes128 = Bytes128.of("Thats my Kung Fu")
    val table16x16Encode = Table16x16.getAesSubstitutionBOX
    val keyExpansion = KeyExpansion128bits(keyBytes128, table16x16Encode)

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of("Two One Nine Two")
    val galoisFieldEncode = Bytes128.galoisFieldEncodeBox

    aesBytes128bitsRegular.addRoundKey(keyExpansion, 0)
    aesBytes128bitsRegular.subBytes(table16x16Encode)
    aesBytes128bitsRegular.shiftRowsEncode()
    aesBytes128bitsRegular.mixColumns(galoisFieldEncode)
    aesBytes128bitsRegular.addRoundKey(keyExpansion, 1)

    val bytesExpected = Array(
      0x58.toByte, 0x15.toByte, 0x59.toByte, 0xcd.toByte,
      0x47.toByte, 0xb6.toByte, 0xd4.toByte, 0x39.toByte,
      0x08.toByte, 0x1c.toByte, 0xe2.toByte, 0xdf.toByte,
      0x8b.toByte, 0xba.toByte, 0xe8.toByte, 0xce.toByte,
    )

    aesBytes128bitsRegular.bytes128.getBytes should be(bytesExpected)

  }

  // ------------------
  // full Encode test:
  // ----------------

  "full ENCODING" should "succeed encoding" in {

    val myAesBlockOfBytes128 = AesBlocksBytes128bitsImplementationRegular.of(
      "Two One Nine Two lol 揦",
      Bytes128.of("Thats my Kung Fu")
    )
    myAesBlockOfBytes128.encodeBlocks()

    myAesBlockOfBytes128.blocks.flatMap(block => block.bytes128.getBytes) should be(Array[Byte](
      0x29.toByte, 0x57.toByte, 0x40.toByte, 0x1a.toByte,
      0xc3.toByte, 0x14.toByte, 0x22.toByte, 0x02.toByte,
      0x50.toByte, 0x20.toByte, 0x99.toByte, 0xd7.toByte,
      0x5f.toByte, 0xf6.toByte, 0xb3.toByte, 0x3a.toByte,
      0x53.toByte, 0xac.toByte, 0xfe.toByte, 0x0b.toByte,
      0xd4.toByte, 0xad.toByte, 0xab.toByte, 0xb4.toByte,
      0x7b.toByte, 0xcc.toByte, 0x73.toByte, 0x77.toByte,
      0x03.toByte, 0x30.toByte, 0xf6.toByte, 0xf6.toByte,
    ))

  }

  // ----------
  // DECODING:
  // -------

  "SubBytes DECODING" should "succeed Decoding" in {

    val array256 = new Array[Byte](256)
    for (i <- array256.indices) {
      array256.update(i, (255 - i).toByte)
    }
    val table16x16Encode = Table16x16(array256)
    val bytes128bitsEncoded = Array(
      0x9e.toByte, 0x9d.toByte, 0x9c.toByte, 0x9b.toByte,
      0x9a.toByte, 0x99.toByte, 0x98.toByte, 0x97.toByte,
      0x96.toByte, 0x95.toByte, 0x94.toByte, 0x93.toByte,
      0x92.toByte, 0x91.toByte, 0x90.toByte, 0x8f.toByte
    )

    val table16x16Decode = Table16x16.createDecodeTable16x16(table16x16Encode)
    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(bytes128bitsEncoded)

    val bytes128bitsDecoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    )

    aesBytes128bitsRegular.subBytes(table16x16Decode)
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsDecoded)

  }

  "ShiftRows DECODING" should "succeed Decoding" in {

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x66.toByte, 0x67.toByte, 0x68.toByte, 0x65.toByte,
      0x6b.toByte, 0x6c.toByte, 0x69.toByte, 0x6a.toByte,
      0x70.toByte, 0x6d.toByte, 0x6e.toByte, 0x6f.toByte
    ))

    val bytes128bitsDecoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    )

    aesBytes128bitsRegular.shiftRowsDecode()
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsDecoded)

  }

  "mixColumns DECODING" should "succeed Decoding" in {

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of(Array(
      0xba.toByte, 0x84.toByte, 0xe8.toByte, 0x1b.toByte,
      0x75.toByte, 0xa4.toByte, 0x8d.toByte, 0x40.toByte,
      0xf4.toByte, 0x8d.toByte, 0x06.toByte, 0x7d.toByte,
      0x7a.toByte, 0x32.toByte, 0x0e.toByte, 0x5d.toByte
    ))
    val galoisFieldDecode = Bytes128.galoisFieldDecodeBox

    val bytes128bitsDecoded = Array(
      0x63.toByte, 0xeb.toByte, 0x9f.toByte, 0xa0.toByte,
      0x2f.toByte, 0x93.toByte, 0x92.toByte, 0xc0.toByte,
      0xaf.toByte, 0xc7.toByte, 0xab.toByte, 0x30.toByte,
      0xa2.toByte, 0x20.toByte, 0xcb.toByte, 0x2b.toByte
    )

    aesBytes128bitsRegular.mixColumns(galoisFieldDecode)
    aesBytes128bitsRegular.bytes128.getBytes should be(bytes128bitsDecoded)
  }

  // ------------------
  // full Decode test:
  // ----------------

  "full DECODING" should "succeed decoding" in {

    val myAesBlockOfBytes128 = AesBlocksBytes128bitsImplementationRegular.of(
      Array[Byte](
        0x29.toByte, 0x57.toByte, 0x40.toByte, 0x1a.toByte,
        0xc3.toByte, 0x14.toByte, 0x22.toByte, 0x02.toByte,
        0x50.toByte, 0x20.toByte, 0x99.toByte, 0xd7.toByte,
        0x5f.toByte, 0xf6.toByte, 0xb3.toByte, 0x3a.toByte,
        0x53.toByte, 0xac.toByte, 0xfe.toByte, 0x0b.toByte,
        0xd4.toByte, 0xad.toByte, 0xab.toByte, 0xb4.toByte,
        0x7b.toByte, 0xcc.toByte, 0x73.toByte, 0x77.toByte,
        0x03.toByte, 0x30.toByte, 0xf6.toByte, 0xf6.toByte,
      ),
      Bytes128.of("Thats my Kung Fu")
    )
    myAesBlockOfBytes128.decodeBlocks()
    myAesBlockOfBytes128.blocks.foreach(block => block.bytes128.reverseBytes128())

    //TODO: improve decode return to string
    val text0 = new String(myAesBlockOfBytes128.blocks(0).bytes128.getBytes, StandardCharsets.UTF_8)
    val text1 = new String(
      new String(myAesBlockOfBytes128.blocks(1).bytes128.getBytes, StandardCharsets.UTF_8)
        .toCharArray
        .filter(c => c != 0x00.toChar)
    )

    text0 should be("Two One Nine Two")
    text1 should be(" lol 揦")

  }

}
