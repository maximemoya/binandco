package fr.maxime.binandco
package tools.aes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable
import scala.collection.mutable.Stack

class AesTest extends AnyFlatSpec with should.Matchers {

  // -------
  // BYTES:
  // ----

  "ShiftRow BYTES" should "succeed Encoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of("abcdefghijklmnop")
    val bytes = bytesBlocks()(0)

    val bytes128bitsEncoded = Array(
      0x61, 0x62, 0x63, 0x64,
      0x66, 0x67, 0x68, 0x65,
      0x6b, 0x6c, 0x69, 0x6a,
      0x70, 0x6d, 0x6e, 0x6f
    )

    bytes.shiftRowEncode()
    bytes() should be(bytes128bitsEncoded)

  }

  "ShiftRow BYTES" should "succeed Decoding" in {

    val bytesBlocks = Bytes128bitsBlocks.of("abcdfgheklijpmno")
    val bytes = bytesBlocks()(0)

    val bytes128bitsEncoded = Array(
      0x61, 0x62, 0x63, 0x64,
      0x65, 0x66, 0x67, 0x68,
      0x69, 0x6a, 0x6b, 0x6c,
      0x6d, 0x6e, 0x6f, 0x70
    )

    bytes.shiftRowDecode()
    bytes() should be(bytes128bitsEncoded)

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
    val table16x16 = Table16x16(array256)

    val intsFormattedEncoded = Array(
      Array(0x9e9e9e9e, 0x9d9d9d9d, 0x9c9c9c9c, 0x9b9b9b9b),
      Array(0x9a9a9a9a, 0x99999999, 0x98989898, 0x97979797),
      Array(0x96969696, 0x95959595, 0x94949494, 0x93939393),
      Array(0x92929292, 0x91919191, 0x90909090, 0x8f8f8f8f)
    )

    AesTools.subBytes(intsFormatted, table16x16)
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
