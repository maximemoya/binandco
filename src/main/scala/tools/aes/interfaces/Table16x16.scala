package fr.maxime.binandco
package tools.aes.interfaces

import scala.util.Random

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

  def getAesSubstitutionBOX: Table16x16 =
    new Table16x16(Array[Byte](
      0x63.toByte, 0x7C.toByte, 0x77.toByte, 0x7B.toByte, 0xF2.toByte, 0x6B.toByte, 0x6F.toByte, 0xC5.toByte, 0x30.toByte, 0x01.toByte, 0x67.toByte, 0x2B.toByte, 0xFE.toByte, 0xD7.toByte, 0xAB.toByte, 0x76.toByte,
      0xCA.toByte, 0x82.toByte, 0xC9.toByte, 0x7D.toByte, 0xFA.toByte, 0x59.toByte, 0x47.toByte, 0xF0.toByte, 0xAD.toByte, 0xD4.toByte, 0xA2.toByte, 0xAF.toByte, 0x9C.toByte, 0xA4.toByte, 0x72.toByte, 0xC0.toByte,
      0xB7.toByte, 0xFD.toByte, 0x93.toByte, 0x26.toByte, 0x36.toByte, 0x3F.toByte, 0xF7.toByte, 0xCC.toByte, 0x34.toByte, 0xA5.toByte, 0xE5.toByte, 0xF1.toByte, 0x71.toByte, 0xD8.toByte, 0x31.toByte, 0x15.toByte,
      0x04.toByte, 0xC7.toByte, 0x23.toByte, 0xC3.toByte, 0x18.toByte, 0x96.toByte, 0x05.toByte, 0x9A.toByte, 0x07.toByte, 0x12.toByte, 0x80.toByte, 0xE2.toByte, 0xEB.toByte, 0x27.toByte, 0xB2.toByte, 0x75.toByte,
      0x09.toByte, 0x83.toByte, 0x2C.toByte, 0x1A.toByte, 0x1B.toByte, 0x6E.toByte, 0x5A.toByte, 0xA0.toByte, 0x52.toByte, 0x3B.toByte, 0xD6.toByte, 0xB3.toByte, 0x29.toByte, 0xE3.toByte, 0x2F.toByte, 0x84.toByte,
      0x53.toByte, 0xD1.toByte, 0x00.toByte, 0xED.toByte, 0x20.toByte, 0xFC.toByte, 0xB1.toByte, 0x5B.toByte, 0x6A.toByte, 0xCB.toByte, 0xBE.toByte, 0x39.toByte, 0x4A.toByte, 0x4C.toByte, 0x58.toByte, 0xCF.toByte,
      0xD0.toByte, 0xEF.toByte, 0xAA.toByte, 0xFB.toByte, 0x43.toByte, 0x4D.toByte, 0x33.toByte, 0x85.toByte, 0x45.toByte, 0xF9.toByte, 0x02.toByte, 0x7F.toByte, 0x50.toByte, 0x3C.toByte, 0x9F.toByte, 0xA8.toByte,
      0x51.toByte, 0xA3.toByte, 0x40.toByte, 0x8F.toByte, 0x92.toByte, 0x9D.toByte, 0x38.toByte, 0xF5.toByte, 0xBC.toByte, 0xB6.toByte, 0xDA.toByte, 0x21.toByte, 0x10.toByte, 0xFF.toByte, 0xF3.toByte, 0xD2.toByte,
      0xCD.toByte, 0x0C.toByte, 0x13.toByte, 0xEC.toByte, 0x5F.toByte, 0x97.toByte, 0x44.toByte, 0x17.toByte, 0xC4.toByte, 0xA7.toByte, 0x7E.toByte, 0x3D.toByte, 0x64.toByte, 0x5D.toByte, 0x19.toByte, 0x73.toByte,
      0x60.toByte, 0x81.toByte, 0x4F.toByte, 0xDC.toByte, 0x22.toByte, 0x2A.toByte, 0x90.toByte, 0x88.toByte, 0x46.toByte, 0xEE.toByte, 0xB8.toByte, 0x14.toByte, 0xDE.toByte, 0x5E.toByte, 0x0B.toByte, 0xDB.toByte,
      0xE0.toByte, 0x32.toByte, 0x3A.toByte, 0x0A.toByte, 0x49.toByte, 0x06.toByte, 0x24.toByte, 0x5C.toByte, 0xC2.toByte, 0xD3.toByte, 0xAC.toByte, 0x62.toByte, 0x91.toByte, 0x95.toByte, 0xE4.toByte, 0x79.toByte,
      0xE7.toByte, 0xC8.toByte, 0x37.toByte, 0x6D.toByte, 0x8D.toByte, 0xD5.toByte, 0x4E.toByte, 0xA9.toByte, 0x6C.toByte, 0x56.toByte, 0xF4.toByte, 0xEA.toByte, 0x65.toByte, 0x7A.toByte, 0xAE.toByte, 0x08.toByte,
      0xBA.toByte, 0x78.toByte, 0x25.toByte, 0x2E.toByte, 0x1C.toByte, 0xA6.toByte, 0xB4.toByte, 0xC6.toByte, 0xE8.toByte, 0xDD.toByte, 0x74.toByte, 0x1F.toByte, 0x4B.toByte, 0xBD.toByte, 0x8B.toByte, 0x8A.toByte,
      0x70.toByte, 0x3E.toByte, 0xB5.toByte, 0x66.toByte, 0x48.toByte, 0x03.toByte, 0xF6.toByte, 0x0E.toByte, 0x61.toByte, 0x35.toByte, 0x57.toByte, 0xB9.toByte, 0x86.toByte, 0xC1.toByte, 0x1D.toByte, 0x9E.toByte,
      0xE1.toByte, 0xF8.toByte, 0x98.toByte, 0x11.toByte, 0x69.toByte, 0xD9.toByte, 0x8E.toByte, 0x94.toByte, 0x9B.toByte, 0x1E.toByte, 0x87.toByte, 0xE9.toByte, 0xCE.toByte, 0x55.toByte, 0x28.toByte, 0xDF.toByte,
      0x8C.toByte, 0xA1.toByte, 0x89.toByte, 0x0D.toByte, 0xBF.toByte, 0xE6.toByte, 0x42.toByte, 0x68.toByte, 0x41.toByte, 0x99.toByte, 0x2D.toByte, 0x0F.toByte, 0xB0.toByte, 0x54.toByte, 0xBB.toByte, 0x16.toByte
    ))

  def transformByteAccordingTable16x16(byte: Byte, table: Table16x16): Byte = {

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

}
