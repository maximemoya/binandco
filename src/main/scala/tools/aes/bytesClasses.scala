package fr.maxime.binandco
package tools.aes

import java.nio.charset.Charset

/**
 * {{{
 *   GET bytes Array formatted by 4 Bytes length multiple
 *   BY Applying an instance of this class
 *   example:
 *      textBytes 0x[6d 6d]
 *        will produce an Bytes4Formatted => 0x[6d 6d 00 00]
 *      textBytes 0x[6d 6d 6d 6d 6d]
 *        will produce an Bytes4Formatted => 0x[6d 6d 6d 6d 6d 00 00 00]
 * }}}
 *
 * @param text String
 */
class Bytes4Formatted(text: String) {

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

class IntsFormatted(bytes4Formatted: Bytes4Formatted, packetSize: Int) {

  def apply(): Array[Array[Int]] = ints

  override def toString: String = {
    s"n2Ints: \n0x[\n${ints.map(_.map(String.format(s"%08x", _)).mkString(" ")).mkString("\n")}\n]"
  }

  private val ints: Array[Array[Int]] = init()

  private def init(): Array[Array[Int]] = {
    val bytes = bytes4Formatted()
    val intsLength = (((bytes.length - 1) / (packetSize * 4)) + 1) * packetSize
    val ints: Array[Int] = new Array[Int](intsLength)
    println(s"Array ints length: $intsLength")
    for (i <- 0 until (bytes.length / 4)) {
      val bufferBytes: Array[Byte] = new Array[Byte](4)
      for (j <- bufferBytes.indices) {
        bufferBytes.update(j, bytes(j + i * 4))
      }
      ints.update(i, BigInt.apply(bufferBytes).toInt)
    }
    println(s"ints = 0x[${ints.map(int => String.format("%08x", int)).mkString(" ")}]")

    val n2IntsLength = intsLength / packetSize
    val n2Ints = new Array[Array[Int]](n2IntsLength)
    for (i <- n2Ints.indices) {
      val bufferInts = new Array[Int](packetSize)
      for (j <- bufferInts.indices) {
        bufferInts.update(j, ints(j + i * packetSize))
      }
      n2Ints.update(i, bufferInts)
    }

    println(s"n2Ints: \n0x[\n${n2Ints.map(_.map(String.format(s"%08x", _)).mkString(" ")).mkString("\n")}\n]")
    n2Ints
  }

}
