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
    val bytesBlocks = Bytes128bitsBlocks.of(Array[Byte](
      0x0f, 0x15, 0x71, 0xc9.toByte,
      0x47, 0xd9.toByte, 0xe8.toByte, 0x59,
      0x0c, 0xb7.toByte, 0xad.toByte, 0xd6.toByte,
      0xaf.toByte, 0x7f, 0x67.toByte, 0x98.toByte,
    ))
    val bytes128bits = bytesBlocks(0)

    val word_04 = 0xdc9037b0
    val word_05 = 0x9b49dfe9
    val word_06 = 0x97fe723f
    val word_07 = 0x388115a7
    val word_40 = 0xb48ef352
    val word_41 = 0xba98134e
    val word_42 = 0x7f4d5920
    val word_43 = 0x86261876

    val keyExpansionTest = keyExpansionAES128(bytes128bits, sBox)
    keyExpansionTest(4) should be(word_04)
    keyExpansionTest(5) should be(word_05)
    keyExpansionTest(6) should be(word_06)
    keyExpansionTest(7) should be(word_07)
    keyExpansionTest(40) should be(word_40)
    keyExpansionTest(41) should be(word_41)
    keyExpansionTest(42) should be(word_42)
    keyExpansionTest(43) should be(word_43)

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
