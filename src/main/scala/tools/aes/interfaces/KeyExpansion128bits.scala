package fr.maxime.binandco
package tools.aes.interfaces

private class KeyExpansion128bits(key128bits: Bytes128, table16x16: Table16x16) {

  val words: Array[Array[Int]] = getKeyExpansion(key128bits, table16x16)

  def apply(round: Int): Array[Int] = words(round)

  def printWords(): Unit = {
    println(s" < KeyExpansion128bits (${words.length} words) > :")
    println(words.map(word => word.map(i => "\t" + String.format("%8s", i.toHexString).replace(" ", "0")).mkString("\n")).mkString("\n\n"))
    println(" < end KeyExpansion128bits >\n")
  }

  private def getKeyExpansion(key128bits: Bytes128, table16x16: Table16x16) = {
    val bytesW1 = Array[Byte](key128bits(0), key128bits(1), key128bits(2), key128bits(3))
    val word1: Int = BigInt.apply(bytesW1).toInt
    val bytesW2 = Array[Byte](key128bits(4), key128bits(5), key128bits(6), key128bits(7))
    val word2: Int = BigInt.apply(bytesW2).toInt
    val bytesW3 = Array[Byte](key128bits(8), key128bits(9), key128bits(10), key128bits(11))
    val word3: Int = BigInt.apply(bytesW3).toInt
    val bytesW4 = Array[Byte](key128bits(12), key128bits(13), key128bits(14), key128bits(15))
    val word4: Int = BigInt.apply(bytesW4).toInt

    val arrayKeyExpansion: Array[Int] = new Array[Int](4 * 11)
    arrayKeyExpansion.update(0, word1)
    arrayKeyExpansion.update(1, word2)
    arrayKeyExpansion.update(2, word3)
    arrayKeyExpansion.update(3, word4)

    val rCons = KeyExpansion128bits.getRoundConstants
    for (i <- 0 until 10) {

      val wordRot = rotationWord(arrayKeyExpansion(3 + 4 * i))
      val subBytes = new Array[Byte](4)
      for (j <- subBytes.indices) {
        val byte = Table16x16.transformByteAccordingTable16x16(KeyExpansion128bits.intToByte(wordRot, j), table16x16)
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

    val arrayKeyExpansionReversed = new Array[Int](arrayKeyExpansion.length)
    for (i <- arrayKeyExpansionReversed.indices) {
      if (i % 4 == 0) {
        val b0 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i), 0)
        val b1 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 1), 0)
        val b2 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 2), 0)
        val b3 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 3), 0)
        val arr = Array[Byte](b0, b1, b2, b3)
        arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
      }
      else if (i % 4 == 1) {
        val b0 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 1), 1)
        val b1 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i), 1)
        val b2 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 1), 1)
        val b3 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 2), 1)
        val arr = Array[Byte](b0, b1, b2, b3)
        arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
      }
      else if (i % 4 == 2) {
        val b0 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 2), 2)
        val b1 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 1), 2)
        val b2 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i), 2)
        val b3 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i + 1), 2)
        val arr = Array[Byte](b0, b1, b2, b3)
        arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
      }
      else if (i % 4 == 3) {
        val b0 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 3), 3)
        val b1 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 2), 3)
        val b2 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i - 1), 3)
        val b3 = KeyExpansion128bits.intToByte(arrayKeyExpansion(i), 3)
        val arr = Array[Byte](b0, b1, b2, b3)
        arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
      }
    }

    val arrayKeyExpansionReversedN2 = new Array[Array[Int]](11)
    for (i <- arrayKeyExpansionReversedN2.indices) {
      arrayKeyExpansionReversedN2.update(i, Array(
        arrayKeyExpansionReversed(i * 4),
        arrayKeyExpansionReversed(i * 4 + 1),
        arrayKeyExpansionReversed(i * 4 + 2),
        arrayKeyExpansionReversed(i * 4 + 3)
      ))

    }

    arrayKeyExpansionReversedN2

  }

  private def rotationWord(word: Int): Int = {
    val a = word << 8
    val b = word >>> 24
    a ^ b
  }

}

object KeyExpansion128bits {
  def of(key128bits: Bytes128, table16x16: Table16x16): KeyExpansion128bits = {
    new KeyExpansion128bits(key128bits, table16x16)
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

}
