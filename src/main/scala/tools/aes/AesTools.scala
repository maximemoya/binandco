package fr.maxime.binandco
package tools.aes

import tools.aes.utils.Bytes128bits

import java.nio.charset.Charset
import scala.annotation.unused
import scala.compiletime.ops.boolean.||
import scala.util.Random

/**
 * {{{
 *   apply() => GET Array[Byte] formatted by multiple of 4
 *   example:
 *      text = "mm" (0x[6d 6d])
 *        will produce a Bytes4Formatted => 0x[6d 6d 00 00]
 *      text = "mmmmm" (0x[6d 6d 6d 6d 6d])
 *        will produce a Bytes4Formatted => 0x[6d 6d 6d 6d 6d 00 00 00]
 * }}}
 *
 * @param text String
 */
class BytesMultipleOf4(text: String) {

  def apply(): Array[Byte] = bytes

  private val bytes: Array[Byte] = init()

  private def init(): Array[Byte] = {
    val textBytes = text.getBytes(Charset.forName("UTF-8"))

    //    println(s"Array text bytesN2 length: ${textBytes.length}")
    //    println(s"textBytes = 0x[${textBytes.map(byte => String.format("%02x", byte)).mkString(" ")}]")

    val bytesLength = (((textBytes.length - 1) / 4) + 1) * 4
    val bytes: Array[Byte] = new Array[Byte](bytesLength)
    for (i <- textBytes.indices) {
      bytes.update(i, textBytes(i))
    }
    bytes
  }
}

/**
 * {{{
 *   apply() => Array[Array[Int]] formatted by 'packetSize' Array[Int]
 *   example:
 *      bytes4Formatted = 0x[6d, 6d, 6d, 6d, 6d, 00, 00, 00]
 *      packetSize = 1 (1*4 Bytes)
 *        will produce an IntsFormatted =>
 *           [
 *            [0x(6d 6d 6d 6d)],
 *            [0x(6d 6d 00 00)]
 *           ]
 *      bytes4Formatted = 0x[6d, 6d, 6d, 6d, 6d, 00, 00, 00]
 *      packetSize = 3 (3*4 Bytes)
 *        will produce an IntsFormatted =>
 *           [
 *            [0x(6d 6d 6d 6d), 0x(6d 00 00 00), 0x(00 00 00 00)]
 *           ]
 * }}}
 *
 * @param bytes4Formatted Bytes4Formatted
 * @param packetSize      quantity of Array[Int]
 */
class IntsFormatted(bytes4Formatted: BytesMultipleOf4, packetSize: Int) {

  def apply(): Array[Array[Int]] = ints

  override def toString: String = {
    s"n2Ints: \n0x[\n${ints.map(_.map(String.format(s"%08x", _)).mkString(" ")).mkString("\n")}\n]"
  }

  private val ints: Array[Array[Int]] = init()

  private def init(): Array[Array[Int]] = {
    val bytes = bytes4Formatted()
    val intsLength = (((bytes.length - 1) / (packetSize * 4)) + 1) * packetSize
    val ints: Array[Int] = new Array[Int](intsLength)

    //    println(s"Array ints length: $intsLength")

    for (i <- 0 until (bytes.length / 4)) {
      val bufferBytes: Array[Byte] = new Array[Byte](4)
      for (j <- bufferBytes.indices) {
        bufferBytes.update(j, bytes(j + i * 4))
      }
      ints.update(i, BigInt.apply(bufferBytes).toInt)
    }

    //    println(s"ints = 0x[${ints.map(int => String.format("%08x", int)).mkString(" ")}]")

    val n2IntsLength = intsLength / packetSize
    val n2Ints = new Array[Array[Int]](n2IntsLength)
    for (i <- n2Ints.indices) {
      val bufferInts = new Array[Int](packetSize)
      for (j <- bufferInts.indices) {
        bufferInts.update(j, ints(j + i * packetSize))
      }
      n2Ints.update(i, bufferInts)
    }

    //    println(s"n2Ints: \n0x[\n${n2Ints.map(_.map(String.format(s"%08x", _)).mkString(" ")).mkString("\n")}\n]")

    n2Ints
  }

}

/**
 * {{{
 *   apply() => Array[Byte] table 256 Bytes (16x16)
 * }}}
 *
 * @param array256Bytes Array[Byte]
 */
class Table16x16(array256Bytes: Array[Byte]) {

  // --------
  // Public:
  // ------
  def apply(): Array[Byte] = table

  def get(x: Int, y: Int): Byte = table(x + y * 16)

  def get(n: Int): Byte = table(n)

  override def toString: String = tableString

  // ---------
  // Private:
  // -------
  private val table: Array[Byte] = initTable()
  private val tableString: String = initTableString()

  private def initTable() = {
    val returnedArray = new Array[Byte](256)
    if (array256Bytes.length >= 256) {
      for (i <- returnedArray.indices) {
        returnedArray.update(i, array256Bytes(i))
      }
    }
    else {
      for (i <- array256Bytes.indices) {
        returnedArray.update(i, array256Bytes(i))
      }
      //      for (i <- array256Bytes.length until returnedArray.length){
      //        returnedArray.update(i,0x00)
      //      }
    }
    returnedArray
  }

  private def initTableString() = {
    val arrayString = new Array[String](table.length)
    for (i <- table.indices) {
      if (i % 16 == 0) {
        arrayString.update(i, s"\n${String.format("%02x", table(i))}")
      }
      else {
        arrayString.update(i, s"${String.format("%02x", table(i))}")
      }
    }
    s"${arrayString.mkString(" ")}\n"
  }

}

object Table16x16 {

  def createEncodeTable16x16Random(): Table16x16 = {
    val arrLength = 16 * 16
    val array = new Array[Byte](arrLength)
    for (i <- array.indices) {
      array.update(i, i.toByte)
    }
    new Table16x16(Random.shuffle(array).toArray)
  }

  def createDecodeTable16x16(tableEncode: Table16x16): Table16x16 = {

    val array = new Array[Byte](256)
    for (i <- tableEncode().indices) {
      val byte = tableEncode()(i)
      val byteHex = String.format("%02x", byte)
      val line = Integer.parseInt(byteHex(0).toString, 16)
      val column = Integer.parseInt(byteHex(1).toString, 16)
      val index = line * 16 + column

      array.update(index, i.toByte)
    }
    new Table16x16(array)
  }

}

object AesTools {

  // ----------
  // SubBytes:
  // --------

  private def transformByteAccordingTable16x16(byte: Byte, table: Table16x16): Byte = {

    //    val str = String.format("%02x", byte)
    //    val line = Integer.parseInt(str(0).toString, 16)
    //    val column = Integer.parseInt(str(1).toString, 16)
    //
    //    println(s"byte= $byte | 0x${String.format("%02x", byte)}")
    //    println(s"str= \"$str\"")
    //    println(s"line= ${line + 1}")
    //    println(s"column= ${column + 1}")

    var indexToReach: Int = byte
    if (byte < 0) indexToReach = byte + 256

    //    println(s"arr($indexToReach)=${String.format("%02x", table.get(indexToReach))}")

    table.get(indexToReach)

  }

  private def transformIntAccordingTable16x16(int: Int, table: Table16x16): Int = {
    val bytes = BigInt.apply(int).toByteArray

    //    println(s"before bytesN2 = 0x[${bytesN2.map(b => String.format("%02x", b)).mkString(" ")}]")

    for (i <- bytes.indices) {
      val newByte = transformByteAccordingTable16x16(bytes(i), table)
      bytes.update(i, newByte)
    }

    //    println(s"after bytesN2  = 0x[${bytesN2.map(b => String.format("%02x", b)).mkString(" ")}]")

    BigInt.apply(bytes).toInt
  }

  def subBytes(intsFormatted: IntsFormatted, tableEncode: Table16x16): Unit = {
    for (line <- intsFormatted().indices) {
      for (column <- intsFormatted()(line).indices) {
        val oldInt = intsFormatted()(line)(column)
        val newInt = transformIntAccordingTable16x16(oldInt, tableEncode)
        intsFormatted()(line).update(column, newInt)
      }
    }
  }

  // ----------
  // ShiftRow:
  // --------

  private def get3LastArrayCopied(intsFormatted: IntsFormatted): (Array[Int], Array[Int], Array[Int]) = {

    val copy1 = new Array[Int](4)
    for (i <- intsFormatted()(1).indices) {
      copy1.update(i, intsFormatted()(1)(i))
    }

    val copy2 = new Array[Int](4)
    for (i <- intsFormatted()(2).indices) {
      copy2.update(i, intsFormatted()(2)(i))
    }

    val copy3 = new Array[Int](4)
    for (i <- intsFormatted()(3).indices) {
      copy3.update(i, intsFormatted()(3)(i))
    }

    (copy1, copy2, copy3)
  }

  /**
   * {{{
   *   shift row's of an IntsFormatted (4x4: Array[Array[Int]]):
   *    from => [
   *        [0,1,2,3],
   *        [0,1,2,3], rotate left by 1
   *        [0,1,2,3], rotate left by 2
   *        [0,1,2,3]  rotate left by 3
   *       ]
   *    encoded => [
   *        [0,1,2,3],
   *        [1,2,3,0],
   *        [2,3,0,1],
   *        [3,0,1,2]
   *       ]
   * }}}
   *
   * @param intsFormatted Array(Array[Int] of 4 Array[Int] of 4 Int
   */
  def shiftRowEncode(intsFormatted: IntsFormatted): Unit = {

    val (copy1, copy2, copy3) = get3LastArrayCopied(intsFormatted)
    for (i <- 1 until intsFormatted().length) {
      if (i == 1) {
        for (j <- intsFormatted()(i).indices) {
          if (j < intsFormatted()(i).length - 1) {
            intsFormatted()(i).update(j, intsFormatted()(i)(j + 1))
          }
          else {
            intsFormatted()(i).update(j, copy1(0))
          }
        }
      }
      else if (i == 2) {
        for (j <- intsFormatted()(i).indices) {
          if (j < intsFormatted()(i).length - 2) {
            intsFormatted()(i).update(j, intsFormatted()(i)(j + 2))
          }
          else if (j < intsFormatted()(i).length - 1) {
            intsFormatted()(i).update(j, copy2(0))
          }
          else {
            intsFormatted()(i).update(j, copy2(1))
          }
        }
      }
      else {
        for (j <- intsFormatted()(i).indices) {
          if (j < intsFormatted()(i).length - 3) {
            intsFormatted()(i).update(j, intsFormatted()(i)(j + 3))
          }
          else if (j < intsFormatted()(i).length - 2) {
            intsFormatted()(i).update(j, copy3(0))
          }
          else if (j < intsFormatted()(i).length - 1) {
            intsFormatted()(i).update(j, copy3(1))
          }
          else {
            intsFormatted()(i).update(j, copy3(2))
          }
        }
      }
    }

  }

  /**
   * {{{
   *   shift row's of an IntsFormatted (4x4: Array[Array[Int]]):
   *    from => [
   *        [0,1,2,3],
   *        [1,2,3,0], rotate right by 1
   *        [2,3,0,1], rotate right by 2
   *        [3,0,1,2]  rotate right by 3
   *       ]
   *    decoded => [
   *        [0,1,2,3],
   *        [0,1,2,3],
   *        [0,1,2,3],
   *        [0,1,2,3]
   *       ]
   * }}}
   *
   * @param intsFormatted Array(Array[Int] of 4 Array[Int] of 4 Int
   */
  def shiftRowDecode(intsFormatted: IntsFormatted): Unit = {

    val (copy1, copy2, copy3) = get3LastArrayCopied(intsFormatted)
    for (i <- 1 until intsFormatted().length) {
      if (i == 1) {
        for (j <- intsFormatted().indices) {
          if (j == 0) {
            intsFormatted()(i).update(j, copy1(3))
          }
          else {
            intsFormatted()(i).update(j, copy1(j - 1))
          }
        }
      }
      else if (i == 2) {
        for (j <- intsFormatted()(i).indices) {
          if (j < intsFormatted()(i).length - 2) {
            intsFormatted()(i).update(j, copy2(j + 2))
          }
          else if (j < intsFormatted()(i).length - 1) {
            intsFormatted()(i).update(j, copy2(0))
          }
          else {
            intsFormatted()(i).update(j, copy2(1))
          }
        }
      }
      else {
        for (j <- intsFormatted()(i).indices) {
          if (j < intsFormatted()(i).length - 1) {
            intsFormatted()(i).update(j, intsFormatted()(i)(j + 1))
          }
          else {
            intsFormatted()(i).update(j, copy3(0))
          }
        }
      }
    }

  }

}
