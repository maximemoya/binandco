package fr.maxime.binandco
package tools.aes

import tools.aes.{Aes, AesTools}
import tools.sha.Sha256

import java.nio.charset.Charset

object Aes {

  def encodeTextWithKey(plaintext: String, keyText: String): Unit = {

    val key256bits = Sha256.hash(keyText)
    println(s"key256bits $key256bits")

    val bytes4Formatted = Bytes4Formatted(key256bits)
    val intsFormatted4x4Ints = IntsFormatted(bytes4Formatted, 4)
    println(s"\n$intsFormatted4x4Ints\n")

    AesTools.shiftRowEncode(intsFormatted4x4Ints)
    println(s"shiftRowEncode:\n$intsFormatted4x4Ints\n")
    AesTools.shiftRowDecode(intsFormatted4x4Ints)
    println(s"shiftRowDecode:\n$intsFormatted4x4Ints\n")

    val encodeTable256 = AesTools.createRandomTable16x16()
    println("encodeTable256:")
    println(encodeTable256)
    val decodeTable256 = AesTools.getDecodeTable16x16(encodeTable256)
    println("decodeTable256:")
    println(decodeTable256)

    println("mixColumnEncode:")
    AesTools.mixColumnEncode(intsFormatted4x4Ints, encodeTable256)
    println(intsFormatted4x4Ints)

    println("mixColumnDecode:")
    AesTools.mixColumnDecode(intsFormatted4x4Ints, decodeTable256)
    println(intsFormatted4x4Ints)

  }

}

object TryIt extends App {
  Aes.encodeTextWithKey("mm", "test")

}
