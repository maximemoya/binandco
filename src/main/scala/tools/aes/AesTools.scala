package fr.maxime.binandco
package tools.aes

import java.nio.charset.Charset

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
 * @param packetSize      length of Array[Int]
 */
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

object AesTools {

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
