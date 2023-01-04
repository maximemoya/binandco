package fr.maxime.binandco
package tools

import java.nio.charset.Charset

object Aes {

  private def getBytes4Formatted(text:String): Array[Byte] = {
    println(s"text: $text")
    val textBytes = text.getBytes(Charset.forName("UTF-8"))
    println(s"Array text bytes length: ${textBytes.length}")
    println(s"textBytes = 0x[${textBytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")

    val bytesLength = (((textBytes.length - 1) / 4) + 1) * 4
    val bytes: Array[Byte] = new Array[Byte](bytesLength)
    println(s"Array bytes length: $bytesLength")
    for (i <- textBytes.indices) {
      bytes.update(i, textBytes(i))
    }
//    for (i <- textBytes.length until bytes.length) {
//      bytes.update(i, 0x00)
//    }
    println(s"bytes = 0x[${bytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")
    bytes
  }
  private def getN2Ints(bytes4Formatted: Array[Byte], packetSize: Int): Array[Array[Int]] = {
    val intsLength = (((bytes4Formatted.length - 1) / (packetSize * 4)) + 1) * packetSize
    val ints: Array[Int] = new Array[Int](intsLength)
    println(s"Array ints length: $intsLength")
    for (i <- 0 until (bytes4Formatted.length / 4)) {
      val bufferBytes: Array[Byte] = new Array[Byte](4)
      for (j <- bufferBytes.indices) {
        bufferBytes.update(j, bytes4Formatted(j + i * 4))
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

    println(s"n2Ints: \n0x[\n${n2Ints.map(_.map(String.format(s"%08x",_)).mkString(" ")).mkString("\n")}\n]")
    n2Ints
  }

  def cypher(text: String): Unit = {

    val bytes4Formatted = getBytes4Formatted(text)
    val n2Ints256bits = getN2Ints(bytes4Formatted, 8)

//    println(s"text: $text")
//    val textBytes = text.getBytes(Charset.forName("UTF-8"))
//    println(s"Array text bytes length: ${textBytes.length}")
//    println(s"textBytes = 0x[${textBytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")
//
//    val bytesLength = (((textBytes.length - 1) / 4) + 1) * 4
//    val bytes: Array[Byte] = new Array[Byte](bytesLength)
//    println(s"Array bytes length: $bytesLength")
//    for (i <- textBytes.indices) {
//      bytes.update(i, textBytes(i))
//    }
//    for (i <- textBytes.length until bytes.length) {
//      bytes.update(i, 0x00)
//    }
//    println(s"bytes = 0x[${bytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")

//    val intsLength = (((bytes.length - 1) / 16) + 1) * 4
//    val ints: Array[Int] = new Array[Int](intsLength)
//    println(s"Array ints length: $intsLength")
//    for (i <- 0 until (bytes.length / 4)) {
//      val bufferBytes: Array[Byte] = new Array[Byte](4)
//      for (j <- bufferBytes.indices) {
//        bufferBytes.update(j, bytes(j + i * 4))
//      }
//      ints.update(i, BigInt.apply(bufferBytes).toInt)
//    }
//    println(s"ints = 0x[${ints.map(int => String.format("%08x", int)).mkString(" ")}]")

  }

}

object TestIt extends App {
  Aes.cypher("buy me some potato chips please ;)")
}
