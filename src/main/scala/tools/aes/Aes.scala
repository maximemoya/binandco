package fr.maxime.binandco
package tools.aes

import tools.aes.utils.{Bytes128bits, Bytes128bitsBlocks, keyExpansionAES128}
import tools.aes.{Aes, AesTools}
import tools.sha.Sha256

import java.nio.charset.Charset

object Aes {

  def encodeDecodeTextWithKey(plaintext: String, keyText: String): Unit = {

    //    val key256bits = Sha256.hash(keyText)
    //    println(s"key256bits $key256bits")
    //    val bytes = new Array[Byte](16)
    //    for (i <- bytes.indices) {
    //      bytes.update(i, key256bits.charAt(i + 1).toByte)
    //    }
    //    val bytesBlock = Bytes128bitsBlocks.of(bytes)

    //    val bytesBlockKey = Bytes128bitsBlocks.of("Thats my Kung Fu")
    val bytesKey = "Thats my Kung Fu".getBytes
    val keyExpansion = keyExpansionAES128(bytesKey, Table16x16.getAesSubstitutionBOX)

    val bytesBlock = Bytes128bitsBlocks.of("Two One Nine Two")

    val table16x16Encode = Table16x16.getAesSubstitutionBOX
    val galoisFieldEncode = Bytes128bits.galoisFieldEncodeBox

    println("\nround 1 key:\n")

    for(i <- keyExpansion(0).indices){
      println("\t" + String.format("%8s",keyExpansion(0)(i).toHexString).replace(" ", "0"))
    }

    println(s"\nTEXT bytesBlock:\n")
    bytesBlock.printString()
    bytesBlock(0).addRoundKey(keyExpansion, 0)
    bytesBlock.printString()
    bytesBlock(0).subBytes(table16x16Encode)
    bytesBlock.printString()
    bytesBlock(0).shiftRowsEncode()
    bytesBlock.printString()
    bytesBlock(0).mixColumns(galoisFieldEncode)
    bytesBlock.printString()
    bytesBlock(0).addRoundKey(keyExpansion, 1)
    bytesBlock.printString()

    //    val table16x16Decode = Table16x16.createDecodeTable16x16(table16x16Encode)
    //    val galoisFieldDecode = Bytes128bits.galoisFieldDecodeBox
    //
    //    bytesBlock(0).mixColumns(galoisFieldDecode)
    //    bytesBlock.printString()
    //    bytesBlock(0).shiftRowsDecode()
    //    bytesBlock.printString()
    //    bytesBlock(0).subBytes(table16x16Decode)
    //    bytesBlock.printString()

  }

}

object TryIt extends App {
  Aes.encodeDecodeTextWithKey("mm", "test")

}
