package fr.maxime.binandco
package tools.aes

import java.nio.charset.Charset

/**
 * {{{
 *   GET the bytes Array formatted by 4 Bytes length multiple
 *   BY Applying an instance of this class
 *   example:
 *      textBytes 0x[6d 6d]
 *        will produce an Bytes4Formatted => 0x[6d 6d 00 00]
 *      textBytes 0x[6d 6d 6d 6d 6d]
 *        will produce an Bytes4Formatted => 0x[6d 6d 6d 6d 6d 00 00 00]
 * }}}
 * @param text
 */
class Bytes4Formatted private(text: String) {

  def apply(): Array[Byte] = bytes

  private val bytes: Array[Byte] = init()

  private def init(): Array[Byte] = {
    val textBytes = text.getBytes(Charset.forName("UTF-8"))
    println(s"Array text bytes length: ${textBytes.length}")
    println(s"textBytes = 0x[${textBytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")
    val bytesLength = (((textBytes.length - 1) / 4) + 1) * 4
    val bytes: Array[Byte] = new Array[Byte](bytesLength)
    for (i <- textBytes.indices) {
      bytes.update(i, textBytes(i))
    }
    bytes
  }
}

/**
 *  -> use create() to get an instance of Bytes4Formatted class
 */
object Bytes4Formatted {
  /**
   * CREATE an instance of Bytes4Formatted class
   * @param text string
   * @see Bytes4Formatted
   */
  def create(text: String): Bytes4Formatted = {
    new Bytes4Formatted(text)
  }
}
