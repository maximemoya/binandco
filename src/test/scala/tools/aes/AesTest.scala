package fr.maxime.binandco
package tools.aes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable
import scala.collection.mutable.Stack

class AesTest extends AnyFlatSpec with should.Matchers {

  "Encoding" should "encode shiftRow" in {

    val bytes = new Bytes4Formatted("aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmmnnnnoooopppp")
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

  "Decoding" should "decode shiftRow" in {

    val bytes = new Bytes4Formatted("aaaabbbbccccddddffffgggghhhheeeekkkklllliiiijjjjppppmmmmnnnnoooo")
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
