package fr.maxime.binandco
package tools.aes.utils

import tools.aes.interfaces.Table16x16
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

    val keyBytes128 = "Thats my Kung Fu".getBytes
    val tableEncode = Table16x16.getAesSubstitutionBOX
    val keyExpansion = keyExpansionAES128(keyBytes128, tableEncode)

    val keyExpansionExpected = Array(
      Array(
        0x54732067,
        0x68204b20,
        0x616d7546,
        0x74796e75
      ),
      Array(
        0xe291b1d6,
        0x32125979,
        0xfc91e4a2,
        0xf188e693
      ),
      Array(
        0x56c776a0,
        0x081a433a,
        0x20b155f7,
        0x078f69fa
      ),
      Array(
        0xd21563c3,
        0x607a3903,
        0x0dbce91e,
        0xe76801fb
      ),
      Array(
        0xa1b4d714,
        0x12685152,
        0x02be5749,
        0xc9a1a05b
      ),
      Array(
        0xb105d2c6,
        0x29411042,
        0x3b85d29b,
        0x33923269
      ),
      Array(
        0xbdb86aac,
        0x3d7c6c2e,
        0xc247950e,
        0x8715274e
      ),
      Array(
        0xcc741eb2,
        0x96ea86a8,
        0xedaa3f31,
        0x1603246a
      ),
      Array(
        0x8efae456,
        0x51bb3d95,
        0xef457a4b,
        0x2122066c
      ),
      Array(
        0xbf45a1f7,
        0xe25964f1,
        0xbffa80cb,
        0x90b2b4d8
      ),
      Array(
        0x286dcc3b,
        0xfda4c031,
        0xde24a46f,
        0xf84afe26
      )
    )

    keyExpansion should be(keyExpansionExpected)

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
