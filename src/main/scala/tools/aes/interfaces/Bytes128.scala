package fr.maxime.binandco
package tools.aes.interfaces

private class Bytes128(bytes: Array[Byte]) {

  def getBytes: Array[Byte] = bytes

  def apply(i: Int): Byte = bytes(i)

  def update(i: Int, b: Byte): Unit = bytes.update(i, b)

  def indices: Seq[Int] = bytes.indices

  /**
   * {{{
   * from =>
   * 0x[
   * 00,01,02,03, <= line 0 become column 0
   * 04,05,06,07, <= line 1 become column 1
   * 08,09,0a,0b, <= line 2 become column 2
   * 0c,0d,0e,0f  <= line 3 become column 3
   * ]
   * reversed =>
   * 0x[
   * 00,04,08,0c,
   * 01,05,09,0d,
   * 02,06,0a,0e,
   * 03,07,0b,0f
   * ]
   * }}}
   */
  def reverseBytes128(): Unit = {

    val copyBytes = new Array[Byte](16)
    for (i <- bytes.indices) {
      copyBytes.update(i, bytes(i))
    }
    bytes.update(0, copyBytes(0))
    bytes.update(1, copyBytes(4))
    bytes.update(2, copyBytes(8))
    bytes.update(3, copyBytes(12))

    bytes.update(4, copyBytes(1))
    bytes.update(5, copyBytes(5))
    bytes.update(6, copyBytes(9))
    bytes.update(7, copyBytes(13))

    bytes.update(8, copyBytes(2))
    bytes.update(9, copyBytes(6))
    bytes.update(10, copyBytes(10))
    bytes.update(11, copyBytes(14))

    bytes.update(12, copyBytes(3))
    bytes.update(13, copyBytes(7))
    bytes.update(14, copyBytes(11))
    bytes.update(15, copyBytes(15))

  }

  def printBytes128(): Unit = {
    var str = ""
    for (byteIndex <- bytes.indices) {
      if (byteIndex % 4 == 0) {
        str += s"\n\t${String.format("%02x", bytes(byteIndex))} "
      }
      else {
        str += s"${String.format("%02x", bytes(byteIndex))} "
      }
    }
    str += "\n"
    println(str)
  }

}

object Bytes128 {

  def of(s: String): Bytes128 = {
    if (s != null) {
      val arr = s.getBytes
      if (arr.length == 16) {
        new Bytes128(arr)
      }
      else if (arr.length < 16) {
        val arr16 = new Array[Byte](16)
        for (i <- arr.indices) {
          arr16.update(i, arr(i))
        }
        new Bytes128(arr16)
      }
      else {
        throw Error(s"\n\tBytes128.of( Array[Byte] ) <= from String: '$s' Array[Byte].length is ${arr.length} , can not be > 16")
      }
    }
    else {
      throw Error("\n\tBytes128.of( Null ) <= can not have 'string = null'")
    }
  }

  def of(bytes: Array[Byte]): Bytes128 = {
    if (bytes.length == 16) {
      new Bytes128(bytes)
    }
    else if (bytes.length < 16) {
      val arr16 = new Array[Byte](16)
      for (i <- bytes.indices) {
        arr16.update(i, bytes(i))
      }
      new Bytes128(arr16)
    }
    else {
      throw Error(s"\n\tBytes128.of( Array[Byte] ) <= Array[Byte].length is ${bytes.length} , can not be > 16'")
    }
  }

  val galoisFieldEncodeBox: Bytes128 =
    Bytes128.of(Array(
      0x02.toByte, 0x03.toByte, 0x01.toByte, 0x01.toByte,
      0x01.toByte, 0x02.toByte, 0x03.toByte, 0x01.toByte,
      0x01.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte,
      0x03.toByte, 0x01.toByte, 0x01.toByte, 0x02.toByte
    ))

  val galoisFieldDecodeBox: Bytes128 =
    Bytes128.of(Array(
      0x0e.toByte, 0x0b.toByte, 0x0d.toByte, 0x09.toByte,
      0x09.toByte, 0x0e.toByte, 0x0b.toByte, 0x0d.toByte,
      0x0d.toByte, 0x09.toByte, 0x0e.toByte, 0x0b.toByte,
      0x0b.toByte, 0x0d.toByte, 0x09.toByte, 0x0e.toByte
    ))

}
