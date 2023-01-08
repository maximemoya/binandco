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

    AesTools.shiftRowEncode(intsFormatted4x4Ints)
    println(intsFormatted4x4Ints)
    AesTools.shiftRowDecode(intsFormatted4x4Ints)
    println(intsFormatted4x4Ints)

  }

}

object TryIt extends App {
  Aes.encodeTextWithKey("mm", "test")
}
