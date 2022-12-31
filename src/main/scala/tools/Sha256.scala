package fr.maxime.binandco
package tools

object Sha256 {

  def getFormattedBlocks(text: String): Array[Array[Byte]] = {

    val textBytes = text.getBytes
    val textLength = textBytes.length

    val extraByte = 0x80.toByte

    val textLengthBitsLong = textLength.toLong * 8L

    val lengthTempBytes: Array[Byte] = BigInt(textLengthBitsLong).toByteArray
    val lengthBytes: Array[Byte] = new Array[Byte](8)
    for (indexToInvert <- lengthTempBytes.indices) {
      lengthBytes.update(7 - indexToInvert, lengthTempBytes.apply(lengthTempBytes.length - 1 - indexToInvert))
    }

    val totalLengthWithout0 = textLength + 1 + 8

    val blockSize = 64
    val blockNumber = ((totalLengthWithout0 - 1) / blockSize) + 1

    val totalLength = blockSize * blockNumber

    val blocks: Array[Array[Byte]] = new Array[Array[Byte]](blockNumber)

    for (blockIndex <- blocks.indices) {
      val bytes = new Array[Byte](blockSize)
      val n = blockIndex * blockSize
      for (indexLocal <- 0 until blockSize) {
        val indexGlobal = indexLocal + n
        if (indexGlobal < textLength) {
          bytes.update(indexLocal, textBytes.apply(indexGlobal))
        }
        else if (indexGlobal == textLength) {
          bytes.update(indexLocal, extraByte)
        }
        else if (indexGlobal >= (totalLength - 8)) {
          bytes.update(indexLocal, lengthBytes.apply(indexLocal % 8))
        }
      }
      blocks.update(blockIndex, bytes)
    }

    //    println("Info about Block-Formatted:")
    //    val rest0Number = blockSize - (totalLengthWithout0 % blockSize)
    //    println(s" > blocks: $blockNumber * $blockSize = ${blockNumber * blockSize} Bytes")
    //    println(s" > left 0: $rest0Number Bytes")

    blocks
  }

  type RotationInt = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 29 | 30 | 31

  private def rotationIntRight(i: Int, rotation: RotationInt): Int = {
    val a = i >>> rotation
    val b = i << (32 - rotation)
    //      println(s"$i rotate right by $rotation")
    //      Tools.printBinariesInfoAboutInt(i, "i")
    //      Tools.printBinariesInfoAboutInt(a, s"a = i >>> $rotation")
    //      Tools.printBinariesInfoAboutInt(b, s"b = i << (32 - $rotation)")
    //      val c = a ^ b
    //      Tools.printBinariesInfoAboutInt(c, "c = a ^ b")
    a ^ b
  }

  private def sigma0(chunk: Array[Int], wordIndex: Int) = {
    val word1 = chunk.apply(wordIndex - 15)
    rotationIntRight(word1, 7) ^ rotationIntRight(word1, 18) ^ (word1 >>> 3)
  }

  private def sigma1(chunk: Array[Int], wordIndex: Int) = {
    val word14 = chunk.apply(wordIndex - 2)
    rotationIntRight(word14, 17) ^ rotationIntRight(word14, 19) ^ (word14 >>> 10)
  }

  private def calculateWord(chunk: Array[Int], wordIndexToCalculate: Int): Int = {
    chunk.apply(wordIndexToCalculate - 16) + sigma0(chunk, wordIndexToCalculate) + chunk.apply(wordIndexToCalculate - 7) + sigma1(chunk, wordIndexToCalculate)
  }

  def getChunks64Words(formattedBlocks: Array[Array[Byte]]): Array[Array[Int]] = {

    val chunks = new Array[Array[Int]](formattedBlocks.length)

    for (blockIndex <- formattedBlocks.indices) {

      val chunk = new Array[Int](64)

      for (wordIndex <- chunk.indices) {

        if (wordIndex < 16) {

          val wordBytes = new Array[Byte](4)
          for (indexByte <- wordBytes.indices) {
            wordBytes.update(indexByte, formattedBlocks.apply(blockIndex).apply(indexByte + (4 * wordIndex)))
          }

          //          Tools.printN1ByteArray(wordBytes, "wordBytes")

          chunk.update(wordIndex, BigInt(wordBytes).toInt)

        }
        else {

          val word = calculateWord(chunk, wordIndex)
          chunk.update(wordIndex, word)

        }

      }

      chunks.update(blockIndex, chunk)

    }

    chunks

  }

  def getAToH(i: Int) = {

    val kConstants: Array[Int] = Array(
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
      0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
      0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
      0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
      0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
      0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
      0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
      0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
      0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
      0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
      0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
      0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
      0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
      0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
      0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
      0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    )

    val h0: Int = 0x6a09e667
    val h1: Int = 0xbb67ae85
    val h2: Int = 0x3c6ef372
    val h3: Int = 0xa54ff53a
    val h4: Int = 0x510e527f
    val h5: Int = 0x9b05688c
    val h6: Int = 0x1f83d9ab
    val h7: Int = 0x5be0cd19

    var a = h0
    var b = h1
    var c = h2
    var d = h3
    var e = h4
    var f = h5
    var g = h6
    var h = h7

    def zeta0(): Int = {
      rotationIntRight(a, 2) ^ rotationIntRight(a, 13) ^ rotationIntRight(a, 22)
    }

    def zeta1(): Int = {
      rotationIntRight(e, 6) ^ rotationIntRight(e, 11) ^ rotationIntRight(e, 25)
    }

    def choice(): Int = {
      (e & f) ^ ((~e) & g)
    }

    def majority(): Int = {
      (a & b) ^ (a & c) ^ (b & c)
    }

    def temp1(chunk:Array[Int], index: Int): Int = {
      h + zeta1() + choice() + kConstants.apply(index) + chunk.apply(index)
    }

    def temp2(): Int = {
      zeta0() + majority()
    }

  }

}
