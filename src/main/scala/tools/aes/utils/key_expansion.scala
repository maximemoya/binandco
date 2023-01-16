package fr.maxime.binandco
package tools.aes.utils

import tools.aes.Table16x16

def keyExpansionAES128(bytes128bits: Bytes128bits, table16x16: Table16x16): Array[Int] = {
  val bytes = bytes128bits.apply()
  val bytesW1 = Array[Byte](bytes(0), bytes(1), bytes(2), bytes(3))
  val word1: Int = BigInt.apply(bytesW1).toInt
  val bytesW2 = Array[Byte](bytes(4), bytes(5), bytes(6), bytes(7))
  val word2: Int = BigInt.apply(bytesW2).toInt
  val bytesW3 = Array[Byte](bytes(8), bytes(9), bytes(10), bytes(11))
  val word3: Int = BigInt.apply(bytesW3).toInt
  val bytesW4 = Array[Byte](bytes(12), bytes(13), bytes(14), bytes(15))
  val word4: Int = BigInt.apply(bytesW4).toInt

  val arrayKeyExpansion: Array[Int] = new Array[Int](4 * 11)
  arrayKeyExpansion.update(0, word1)
  arrayKeyExpansion.update(1, word2)
  arrayKeyExpansion.update(2, word3)
  arrayKeyExpansion.update(3, word4)

  val rCons = getRoundConstants

  for (i <- 0 until 10) {

    val wordRot = rotationWord(arrayKeyExpansion(3 + 4 * i))
    val subBytes = new Array[Byte](4)
    for (j <- subBytes.indices) {
      val byte = Table16x16.transformByteAccordingTable16x16(intToByte(wordRot, j), table16x16)
      subBytes.update(j, byte)
    }
    val wordSubByte = BigInt.apply(subBytes).toInt
    val wordRCon = wordSubByte ^ rCons(i)
    val wordToAdd = wordRCon ^ arrayKeyExpansion(0 + 4 * i)
    arrayKeyExpansion.update(4 + 4 * i, wordToAdd)
    arrayKeyExpansion.update(5 + 4 * i, arrayKeyExpansion(4 + 4 * i) ^ arrayKeyExpansion(1 + 4 * i))
    arrayKeyExpansion.update(6 + 4 * i, arrayKeyExpansion(5 + 4 * i) ^ arrayKeyExpansion(2 + 4 * i))
    arrayKeyExpansion.update(7 + 4 * i, arrayKeyExpansion(6 + 4 * i) ^ arrayKeyExpansion(3 + 4 * i))

  }

  arrayKeyExpansion

}

def rotationWord(word: Int): Int = {
  val a = word << 8
  val b = word >>> 24
  a ^ b
}

def longToByte(along: Long, indexByte0to7: Int): Byte = {
  if (indexByte0to7 >= 0 && indexByte0to7 < 8) {
    val shift = 56 - indexByte0to7 * 8
    val andValue = 0xffL << shift
    ((along & andValue) >>> shift).toByte
  }
  else {
    throw new Error(s"ERROR longToByte(aLong, indexByte0to7) => indexByte0to7: '$indexByte0to7' out of range [0->7]")
  }
}

def intToByte(anInt: Int, indexByte0to3: Int): Byte = {
  if (indexByte0to3 >= 0 && indexByte0to3 < 4) {
    val shift = 24 - indexByte0to3 * 8
    val andValue = 0xffL << shift
    ((anInt & andValue) >>> shift).toByte
  }
  else {
    throw new Error(s"ERROR intToByte(anInt, indexByte0to3) => indexByte0to3: '$indexByte0to3' out of range [0->3]")
  }
}

def getRoundConstants: Array[Int] = {
  Array(
    0x01000000,
    0x02000000,
    0x04000000,
    0x08000000,
    0x10000000,
    0x20000000,
    0x40000000,
    0x80000000,
    0x1B000000,
    0x36000000,
    0x6C000000,
    0xD8000000,
    0xAB000000,
    0x4D000000,
    0x9A000000,
  )
}

def calculateRoundConstants(n: Int): Array[Int] = {
  val array = new Array[Int](n)
  var rci = 0
  for (i <- array.indices) {
    if (i == 0) {
      rci = 1
    }
    else if (rci < 0x80) {
      rci = rci * 2
    }
    else {
      rci = (rci * 2) ^ 0x11b
    }
    array.update(i, rci << 24)
  }
  array
}
