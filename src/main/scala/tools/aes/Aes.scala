package fr.maxime.binandco
package tools.aes

import tools.aes.utils.Bytes128bits
import tools.aes.utils.Bytes128bitsBlocks
import tools.aes.{Aes, AesTools}
import tools.sha.Sha256

import java.nio.charset.Charset

object Aes {

  def encodeDecodeTextWithKey(plaintext: String, keyText: String): Unit = {

    val key256bits = Sha256.hash(keyText)
    println(s"key256bits $key256bits")
    val bytes = new Array[Byte](16)
    for (i <- bytes.indices) {
      bytes.update(i, key256bits.charAt(i + 1).toByte)
    }

    val table16x16Encode = Table16x16.createEncodeTable16x16Random()
    val galoisFieldEncode = Bytes128bits.galoisFieldEncodeBox

    val bytesBlock = Bytes128bitsBlocks.of(bytes)

    bytesBlock.printString()
    bytesBlock.get(0).subBytes(table16x16Encode)
    bytesBlock.printString()
    bytesBlock.get(0).shiftRowsEncode()
    bytesBlock.printString()
    bytesBlock.get(0).mixColumns(galoisFieldEncode)
    bytesBlock.printString()

    val table16x16Decode = Table16x16.createDecodeTable16x16(table16x16Encode)
    val galoisFieldDecode = Bytes128bits.galoisFieldDecodeBox

    bytesBlock.get(0).mixColumns(galoisFieldDecode)
    bytesBlock.printString()
    bytesBlock.get(0).shiftRowsDecode()
    bytesBlock.printString()
    bytesBlock.get(0).subBytes(table16x16Decode)
    bytesBlock.printString()

  }

}

object TryIt extends App {
  Aes.encodeDecodeTextWithKey("mm", "test")

}
