package fr.maxime.binandco
package tools.aes.interfaces

import tools.aes.interfaces.implementations.AesBytes128bitsImplementationRegular

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class KeyExpansion128bitsTest extends AnyFlatSpec with should.Matchers {

  "roundConstant" should "succeed" in {

    KeyExpansion128bits.getRoundConstants should be(KeyExpansion128bits.calculateRoundConstants(15))

  }

  "keyExpansion" should "succeed" in {

    val keyBytes128 = Bytes128.of("Thats my Kung Fu")
    val tableEncode = Table16x16.getAesSubstitutionBOX
    val keyExpansion = KeyExpansion128bits.of(keyBytes128, tableEncode)

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

    keyExpansion.words should be(keyExpansionExpected)

  }

}
