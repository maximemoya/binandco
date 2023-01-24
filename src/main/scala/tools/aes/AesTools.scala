package fr.maxime.binandco
package tools.aes

import tools.aes.interfaces.implementations.{AesBlocksBytes128bitsImplementationRegular, AesBytes128bitsImplementationRegular}
import tools.aes.interfaces.*

import java.nio.charset.StandardCharsets

object AesTools extends App {

  private def testExampleBytes128(): Unit = {

    // ENCODE EXAMPLE:

    val keyBytes128 = Bytes128.of("Thats my Kung Fu")
    val table16x16Encode = Table16x16.getAesSubstitutionBOX
    val keyExpansion = KeyExpansion128bits.of(keyBytes128, table16x16Encode)

    val aesBytes128bitsRegular: AesBytes128bitsInterface = AesBytes128bitsImplementationRegular.of("Two One Nine Two")
    val galoisFieldEncode = Bytes128.galoisFieldEncodeBox

    // ROUND0
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.addRoundKey(keyExpansion, 0)

    // ROUND1
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.subBytes(table16x16Encode)
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.shiftRowsEncode()
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.mixColumns(galoisFieldEncode)
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.addRoundKey(keyExpansion, 1)
    aesBytes128bitsRegular.printBytes()

    // ROUND2
    aesBytes128bitsRegular.subBytes(table16x16Encode)
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.shiftRowsEncode()
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.mixColumns(galoisFieldEncode)
    aesBytes128bitsRegular.printBytes()
    aesBytes128bitsRegular.addRoundKey(keyExpansion, 2)
    aesBytes128bitsRegular.printBytes()

    // ... to continue until ROUND10

  }

  private def testExampleBlockBytes128(): Unit = {

    val keyBytes128 = Bytes128.of("Thats my Kung Fu")
    val aesBytes128bitsRegular: AesBlocksBytes128bitsInterface = AesBlocksBytes128bitsImplementationRegular.of("Two One Nine Two", keyBytes128)
    aesBytes128bitsRegular
      .encodeBlocks()
      .printBlocks()
//    aesBytes128bitsRegular.blocks.foreach(block => block.bytes128.reverseBytes128())
//    aesBytes128bitsRegular.printBlocks()

    aesBytes128bitsRegular
      .decodeBlocks()
      .printBlocks()
//    aesBytes128bitsRegular.blocks.foreach(block => block.bytes128.reverseBytes128())
//    aesBytes128bitsRegular.printBlocks()

    println(new String(aesBytes128bitsRegular.blocks(0).bytes128.getBytes, StandardCharsets.UTF_8))
  }

  testExampleBlockBytes128()

}
