package fr.maxime.binandco
package tools.aes.utils

import tools.aes.Table16x16

def keyExpansionAES128(bytes: Array[Byte], table16x16: Table16x16): Array[Array[Int]] = {
  val bytesW1 = Array[Byte](bytes(0), bytes(4), bytes(8), bytes(12))
  val word1: Int = BigInt.apply(bytesW1).toInt
  val bytesW2 = Array[Byte](bytes(1), bytes(5), bytes(9), bytes(13))
  val word2: Int = BigInt.apply(bytesW2).toInt
  val bytesW3 = Array[Byte](bytes(2), bytes(6), bytes(10), bytes(14))
  val word3: Int = BigInt.apply(bytesW3).toInt
  val bytesW4 = Array[Byte](bytes(3), bytes(7), bytes(11), bytes(15))
  val word4: Int = BigInt.apply(bytesW4).toInt

  val arrayKeyExpansion: Array[Int] = new Array[Int](4 * 11)
  arrayKeyExpansion.update(0, word1)
  arrayKeyExpansion.update(1, word2)
  arrayKeyExpansion.update(2, word3)
  arrayKeyExpansion.update(3, word4)
  //  println("arrayKeyExpansion:")
  //  for(i<- arrayKeyExpansion.indices){
  //    if(arrayKeyExpansion(i)!=0){
  //      println(s"${String.format("%8s", arrayKeyExpansion(i).toHexString).replace(" ", "0")}")
  //    }
  //  }

  val rCons = getRoundConstants

  for (i <- 0 until 10) {

    val wordRot = rotationWord(arrayKeyExpansion(3 + 4 * i))
    //    println(s"wordRot: 0x${wordRot.toHexString}")
    val subBytes = new Array[Byte](4)
    for (j <- subBytes.indices) {
      val byte = Table16x16.transformByteAccordingTable16x16(intToByte(wordRot, j), table16x16)
      subBytes.update(j, byte)
    }
    val wordSubByte = BigInt.apply(subBytes).toInt
    //    println(s"wordSubByte: 0x${wordSubByte.toHexString}")
    val wordRCon = wordSubByte ^ rCons(i)
    //    println(s"wordRCon: 0x${wordRCon.toHexString}")
    val wordToAdd = wordRCon ^ arrayKeyExpansion(0 + 4 * i)
    arrayKeyExpansion.update(4 + 4 * i, wordToAdd)
    //    println(s"wordToAdd: 0x${wordToAdd.toHexString}")
    arrayKeyExpansion.update(5 + 4 * i, arrayKeyExpansion(4 + 4 * i) ^ arrayKeyExpansion(1 + 4 * i))
    //    println(s"wordToAdd: 0x${(arrayKeyExpansion(4 + 4 * i) ^ arrayKeyExpansion(1 + 4 * i)).toHexString}")
    arrayKeyExpansion.update(6 + 4 * i, arrayKeyExpansion(5 + 4 * i) ^ arrayKeyExpansion(2 + 4 * i))
    //    println(s"wordToAdd: 0x${(arrayKeyExpansion(5 + 4 * i) ^ arrayKeyExpansion(2 + 4 * i)).toHexString}")
    arrayKeyExpansion.update(7 + 4 * i, arrayKeyExpansion(6 + 4 * i) ^ arrayKeyExpansion(3 + 4 * i))
    //    println(s"wordToAdd: 0x${(arrayKeyExpansion(6 + 4 * i) ^ arrayKeyExpansion(3 + 4 * i)).toHexString}")

  }

  val arrayKeyExpansionReversed = new Array[Int](arrayKeyExpansion.length)
  for (i <- arrayKeyExpansionReversed.indices) {
    if (i % 4 == 0) {
      val b0 = intToByte(arrayKeyExpansion(i), 0)
      val b1 = intToByte(arrayKeyExpansion(i + 1), 0)
      val b2 = intToByte(arrayKeyExpansion(i + 2), 0)
      val b3 = intToByte(arrayKeyExpansion(i + 3), 0)
      val arr = Array[Byte](b0, b1, b2, b3)
      arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
    }
    else if (i % 4 == 1) {
      val b0 = intToByte(arrayKeyExpansion(i - 1), 1)
      val b1 = intToByte(arrayKeyExpansion(i), 1)
      val b2 = intToByte(arrayKeyExpansion(i + 1), 1)
      val b3 = intToByte(arrayKeyExpansion(i + 2), 1)
      val arr = Array[Byte](b0, b1, b2, b3)
      arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
    }
    else if (i % 4 == 2) {
      val b0 = intToByte(arrayKeyExpansion(i - 2), 2)
      val b1 = intToByte(arrayKeyExpansion(i - 1), 2)
      val b2 = intToByte(arrayKeyExpansion(i), 2)
      val b3 = intToByte(arrayKeyExpansion(i + 1), 2)
      val arr = Array[Byte](b0, b1, b2, b3)
      arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
    }
    else if (i % 4 == 3) {
      val b0 = intToByte(arrayKeyExpansion(i - 3), 3)
      val b1 = intToByte(arrayKeyExpansion(i - 2), 3)
      val b2 = intToByte(arrayKeyExpansion(i - 1), 3)
      val b3 = intToByte(arrayKeyExpansion(i), 3)
      val arr = Array[Byte](b0, b1, b2, b3)
      arrayKeyExpansionReversed.update(i, BigInt.apply(arr).toInt)
    }
  }

  //    println("arrayKeyExpansion:")
  //    println(s"${arrayKeyExpansion.map(x => String.format("0x-%8s", x.toHexString).replace(" ","0")).mkString("\n")}")
  //    println("arrayKeyExpansionReversed:")
  //    println(s"${arrayKeyExpansionReversed.map(x => String.format("0x-%8s", x.toHexString).replace(" ", "0")).mkString("\n")}")

  val arrayKeyExpansionReversedN2 = new Array[Array[Int]](11)
  for (i <- arrayKeyExpansionReversedN2.indices) {
    arrayKeyExpansionReversedN2.update(i, Array(
      arrayKeyExpansionReversed(i * 4),
      arrayKeyExpansionReversed(i * 4 + 1),
      arrayKeyExpansionReversed(i * 4 + 2),
      arrayKeyExpansionReversed(i * 4 + 3)
    ))
    println(s"\nRound{$i}\n")
    print(s"${String.format("%8s", arrayKeyExpansion(i * 4).toHexString).replace(" ", "0")}")
    print(s"${String.format("%8s", arrayKeyExpansion(i * 4 + 1).toHexString).replace(" ", "0")}")
    print(s"${String.format("%8s", arrayKeyExpansion(i * 4 + 2).toHexString).replace(" ", "0")}")
    println(s"${String.format("%8s", arrayKeyExpansion(i * 4 + 3).toHexString).replace(" ", "0")}")
    println()
    println(s"${String.format("%8s", arrayKeyExpansionReversed(i * 4).toHexString).replace(" ", "0")}")
    println(s"${String.format("%8s", arrayKeyExpansionReversed(i * 4 + 1).toHexString).replace(" ", "0")}")
    println(s"${String.format("%8s", arrayKeyExpansionReversed(i * 4 + 2).toHexString).replace(" ", "0")}")
    println(s"${String.format("%8s", arrayKeyExpansionReversed(i * 4 + 3).toHexString).replace(" ", "0")}")
  }

  arrayKeyExpansionReversedN2

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
