package fr.maxime.binandco
package tools.aes.interfaces

private class Bytes128(bytes: Array[Byte]) {

  def getBytes: Array[Byte] = bytes

  def apply(i: Int): Byte = bytes(i)

  def update(i: Int, b: Byte): Unit = bytes.update(i, b)

  def indices: Seq[Int] = bytes.indices

  def printString(): Unit = {
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
