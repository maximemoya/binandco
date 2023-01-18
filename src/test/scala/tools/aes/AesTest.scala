package fr.maxime.binandco
package tools.aes

import tools.aes.utils.{Bytes128bits, Bytes128bitsBlocks, keyExpansionAES128}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.nio.charset.Charset
import scala.collection.mutable
import scala.collection.mutable.Stack

class AesTest extends AnyFlatSpec with should.Matchers {

  // -------
  // BYTES:
  // ----

  "SubBytes BYTES" should "succeed Encoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    ))
    val bytes = bytesBlocks()(0)

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

    bytes.subBytes(table16x16Encode)
    bytes() should be(bytes128bitsEncoded)

  }

  "ShiftRows BYTES" should "succeed Encoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    ))
    val bytes = bytesBlocks()(0)

    val bytes128bitsEncoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x66.toByte, 0x67.toByte, 0x68.toByte, 0x65.toByte,
      0x6b.toByte, 0x6c.toByte, 0x69.toByte, 0x6a.toByte,
      0x70.toByte, 0x6d.toByte, 0x6e.toByte, 0x6f.toByte
    )

    bytes.shiftRowsEncode()
    bytes() should be(bytes128bitsEncoded)

  }

  "mixColumns BYTES" should "succeed Encoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of(Array(
      0x63.toByte, 0xeb.toByte, 0x9f.toByte, 0xa0.toByte,
      0x2f.toByte, 0x93.toByte, 0x92.toByte, 0xc0.toByte,
      0xaf.toByte, 0xc7.toByte, 0xab.toByte, 0x30.toByte,
      0xa2.toByte, 0x20.toByte, 0xcb.toByte, 0x2b.toByte
    ))
    val bytes = bytesBlocks()(0)
    val galoisFieldEncode = Bytes128bits.galoisFieldEncodeBox

    val bytes128bitsEncoded = Array(
      0xba.toByte, 0x84.toByte, 0xe8.toByte, 0x1b.toByte,
      0x75.toByte, 0xa4.toByte, 0x8d.toByte, 0x40.toByte,
      0xf4.toByte, 0x8d.toByte, 0x06.toByte, 0x7d.toByte,
      0x7a.toByte, 0x32.toByte, 0x0e.toByte, 0x5d.toByte
    )

    bytes.mixColumns(galoisFieldEncode)
    bytes() should be(bytes128bitsEncoded)
  }

  "SubBytes BYTES" should "succeed Decoding" in {

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
    val bytesBlocks = Bytes128bitsBlocks.of(bytes128bitsEncoded)
    val bytes = bytesBlocks()(0)

    val bytes128bitsDecoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    )

    bytes.subBytes(table16x16Decode)
    bytes() should be(bytes128bitsDecoded)

  }

  "ShiftRows BYTES" should "succeed Decoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of(Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x66.toByte, 0x67.toByte, 0x68.toByte, 0x65.toByte,
      0x6b.toByte, 0x6c.toByte, 0x69.toByte, 0x6a.toByte,
      0x70.toByte, 0x6d.toByte, 0x6e.toByte, 0x6f.toByte
    ))
    val bytes = bytesBlocks()(0)

    val bytes128bitsDecoded = Array(
      0x61.toByte, 0x62.toByte, 0x63.toByte, 0x64.toByte,
      0x65.toByte, 0x66.toByte, 0x67.toByte, 0x68.toByte,
      0x69.toByte, 0x6a.toByte, 0x6b.toByte, 0x6c.toByte,
      0x6d.toByte, 0x6e.toByte, 0x6f.toByte, 0x70.toByte
    )

    bytes.shiftRowsDecode()
    bytes() should be(bytes128bitsDecoded)

  }

  "mixColumns BYTES" should "succeed Decoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of(Array(
      0xba.toByte, 0x84.toByte, 0xe8.toByte, 0x1b.toByte,
      0x75.toByte, 0xa4.toByte, 0x8d.toByte, 0x40.toByte,
      0xf4.toByte, 0x8d.toByte, 0x06.toByte, 0x7d.toByte,
      0x7a.toByte, 0x32.toByte, 0x0e.toByte, 0x5d.toByte
    ))
    val bytes = bytesBlocks()(0)
    val galoisFieldDecode = Bytes128bits.galoisFieldDecodeBox

    val bytes128bitsDecoded = Array(
      0x63.toByte, 0xeb.toByte, 0x9f.toByte, 0xa0.toByte,
      0x2f.toByte, 0x93.toByte, 0x92.toByte, 0xc0.toByte,
      0xaf.toByte, 0xc7.toByte, 0xab.toByte, 0x30.toByte,
      0xa2.toByte, 0x20.toByte, 0xcb.toByte, 0x2b.toByte
    )

    bytes.mixColumns(galoisFieldDecode)
    bytes() should be(bytes128bitsDecoded)
  }

  // ------------------
  // 1stFullRoundTest:
  // --------------

  "1stFullRoundTest" should "succeed encoding" in {

    val keyBytes128 = "Thats my Kung Fu".getBytes
    val table16x16Encode = Table16x16.getAesSubstitutionBOX
    val keyExpansion = keyExpansionAES128(keyBytes128, table16x16Encode)

    //TODO: make a string method to get bytes in matrix mode
    val bytes128Blocks = Bytes128bitsBlocks.of("Two One Nine Two")
    // to check bypass:
    //    val bytes128Blocks = Bytes128bitsBlocks.of(Bytes128bits.of("Two One Nine Two")())
    val galoisFieldEncode = Bytes128bits.galoisFieldEncodeBox

    bytes128Blocks(0).addRoundKey(keyExpansion, 0)
    bytes128Blocks(0).subBytes(table16x16Encode)
    bytes128Blocks(0).shiftRowsEncode()
    bytes128Blocks(0).mixColumns(galoisFieldEncode)
    bytes128Blocks(0).addRoundKey(keyExpansion, 1)

    val bytesExpected = Array(
      0x58.toByte, 0x15.toByte, 0x59.toByte, 0xcd.toByte,
      0x47.toByte, 0xb6.toByte, 0xd4.toByte, 0x39.toByte,
      0x08.toByte, 0x1c.toByte, 0xe2.toByte, 0xdf.toByte,
      0x8b.toByte, 0xba.toByte, 0xe8.toByte, 0xce.toByte,
    )

    bytes128Blocks(0).apply() should be(bytesExpected)

  }

  // ------
  // INTS:
  // ----

  "SubBytes INTS" should "succeed Encoding" in {

    val bytes = new BytesMultipleOf4("aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmmnnnnoooopppp")
    val intsFormatted = new IntsFormatted(bytes, 4)

    val array256 = new Array[Byte](256)
    for (i <- array256.indices) {
      array256.update(i, (255 - i).toByte)
    }
    val table16x16Encode = Table16x16(array256)

    val intsFormattedEncoded = Array(
      Array(0x9e9e9e9e, 0x9d9d9d9d, 0x9c9c9c9c, 0x9b9b9b9b),
      Array(0x9a9a9a9a, 0x99999999, 0x98989898, 0x97979797),
      Array(0x96969696, 0x95959595, 0x94949494, 0x93939393),
      Array(0x92929292, 0x91919191, 0x90909090, 0x8f8f8f8f)
    )

    AesTools.subBytes(intsFormatted, table16x16Encode)
    intsFormatted() should be(intsFormattedEncoded)

  }

  "ShiftRow INTS" should "succeed Encoding" in {

    val bytes = new BytesMultipleOf4("aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmmnnnnoooopppp")
    val intsFormatted = new IntsFormatted(bytes, 4)

    val intsFormattedEncoded = Array(
      Array(0x61616161, 0x62626262, 0x63636363, 0x64646464),
      Array(0x66666666, 0x67676767, 0x68686868, 0x65656565),
      Array(0x6b6b6b6b, 0x6c6c6c6c, 0x69696969, 0x6a6a6a6a),
      Array(0x70707070, 0x6d6d6d6d, 0x6e6e6e6e, 0x6f6f6f6f)
    )

    AesTools.shiftRowEncode(intsFormatted)
    intsFormatted() should be(intsFormattedEncoded)

  }

  "SubBytes INTS" should "succeed Decoding" in {

    val bytes = new BytesMultipleOf4("aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmmnnnnoooopppp")
    val intsFormatted = new IntsFormatted(bytes, 4)

    val array256 = new Array[Byte](256)
    for (i <- array256.indices) {
      array256.update(i, (255 - i).toByte)
    }
    val table16x16 = Table16x16(array256)

    val intsFormattedDecoded = Array(
      Array(0x9e9e9e9e, 0x9d9d9d9d, 0x9c9c9c9c, 0x9b9b9b9b),
      Array(0x9a9a9a9a, 0x99999999, 0x98989898, 0x97979797),
      Array(0x96969696, 0x95959595, 0x94949494, 0x93939393),
      Array(0x92929292, 0x91919191, 0x90909090, 0x8f8f8f8f)
    )

    AesTools.subBytes(intsFormatted, table16x16)
    intsFormatted() should be(intsFormattedDecoded)

  }

  "ShiftRow INTS" should "succeed Decoding" in {

    val bytes = new BytesMultipleOf4("aaaabbbbccccddddffffgggghhhheeeekkkklllliiiijjjjppppmmmmnnnnoooo")
    val intsFormatted = new IntsFormatted(bytes, 4)

    val intsFormattedDecoded = Array(
      Array(0x61616161, 0x62626262, 0x63636363, 0x64646464),
      Array(0x65656565, 0x66666666, 0x67676767, 0x68686868),
      Array(0x69696969, 0x6a6a6a6a, 0x6b6b6b6b, 0x6c6c6c6c),
      Array(0x6d6d6d6d, 0x6e6e6e6e, 0x6f6f6f6f, 0x70707070)
    )

    AesTools.shiftRowDecode(intsFormatted)
    intsFormatted() should be(intsFormattedDecoded)

  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new mutable.Stack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

}
