package fr.maxime.binandco
package tools.aes.utils

import tools.aes.Table16x16
import tools.aes.utils.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class KeyExpansionAes128Test extends AnyFlatSpec with should.Matchers {

  "roundConstant" should "succeed" in {

    getRoundConstants should be(calculateRoundConstants(15))

  }

  "rotationWord" should "succeed" in {

    val i = 0x12345678
    rotationWord(i) should be(0x34567812)

  }

  "keyExpansion" should "succeed" in {

    val sBox = Table16x16.getAesSubstitutionBOX
    val bytesBlocks = Bytes128bitsBlocks.of("Thats my Kung Fu")
    val bytes128bits = bytesBlocks(0)

    val round_01 = Array(
      0xe291b1d6,
      0x32125979,
      0xfc91e4a2,
      0xf188e693
    )
    val round_10 = Array(
      0x286dcc3b,
      0xfda4c031,
      0xde24a46f,
      0xf84afe26
    )

    val keyExpansionTest = keyExpansionAES128(bytes128bits, sBox)
    keyExpansionTest(1) should be(round_01)
    keyExpansionTest(10) should be(round_10)

  }

}

class LongOrIntToByte extends AnyFlatSpec with should.Matchers {

  "longToByte()" should "succeed" in {

    val along = 0x130e88f4af972635L
    val bytes = Array[Byte](
      0x13.toByte, 0x0e.toByte, 0x88.toByte, 0xf4.toByte, 0xaf.toByte, 0x97.toByte, 0x26.toByte, 0x35.toByte)

    for (i <- 0 until 8) {
      val byte = longToByte(along, i)
      byte should be(bytes(i))
    }
    intercept[Error] {
      longToByte(along, -1)
    }
    intercept[Error] {
      longToByte(along, 8)
    }

  }

  "intToByte()" should "succeed" in {

    val anInt = 0x130e88f4
    val bytes = Array[Byte](0x13.toByte, 0x0e.toByte, 0x88.toByte, 0xf4.toByte)

    for (i <- 0 until 4) {
      val byte = intToByte(anInt, i)
      byte should be(bytes(i))
    }
    intercept[Error] {
      intToByte(anInt, -1)
    }
    intercept[Error] {
      intToByte(anInt, 4)
    }

  }

}
