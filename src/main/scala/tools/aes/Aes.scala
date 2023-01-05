package fr.maxime.binandco
package tools.aes

import tools.aes.Aes

import tools.Sha256

import java.nio.charset.Charset

object Aes {
  
  private def shiftRowEncode(intsFormatted: IntsFormatted): Unit = {

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

  private def shiftRowDecode(intsFormatted: IntsFormatted): Unit = {

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

  def cypher(plaintext: String, keyText: String): Unit = {

    val key256bits = Sha256.hash(keyText)
    println(s"key256bits $key256bits")

    val bytes4Formatted = Bytes4Formatted(key256bits)
    val intsFormatted4x4Ints = IntsFormatted(bytes4Formatted, 4)

    shiftRowEncode(intsFormatted4x4Ints)
    println(intsFormatted4x4Ints)
    shiftRowDecode(intsFormatted4x4Ints)
    println(intsFormatted4x4Ints)

  }

}

object TestIt extends App {
  Aes.cypher("mm", "test")
}
