package fr.maxime.binandco
package tools.aes

import tools.aes.utils.Bytes128bitsBlocks
import tools.aes.{Aes, AesTools}
import tools.sha.Sha256

import java.nio.charset.Charset

object Aes {

  def encodeDecodeTextWithKey(plaintext: String, keyText: String): Unit = {

    val key256bits = Sha256.hash(keyText)
    println(s"key256bits $key256bits")
    val bytes = new Array[Byte](16)
    for (i <- bytes.indices) {
      bytes.update(i, key256bits.charAt(i + 1).toByte)
    }

    val table16x16Encode = Table16x16.createEncodeTable16x16Random()

    val bytesBlock = Bytes128bitsBlocks.of(bytes)

    bytesBlock.printString()
    bytesBlock.get(0).subBytes(table16x16Encode)
    bytesBlock.printString()
    bytesBlock.get(0).shiftRowEncode()
    bytesBlock.printString()

    val table16x16Decode = Table16x16.createDecodeTable16x16(table16x16Encode)

    bytesBlock.get(0).shiftRowDecode()
    bytesBlock.printString()
    bytesBlock.get(0).subBytes(table16x16Decode)
    bytesBlock.printString()

  }

}

private def getPolynomial(intMax: Int, strBinMin: String): Byte = {

  println(s"intMax    = ${String.format("%8s", (intMax & 0xff).toBinaryString).replace(' ', '0')}")
  println(s"strBinMin = ${String.format("%8s", strBinMin).replace(' ', '0')}")

  val numbs = new Array[Int](strBinMin.count(c => c == '1'))
  var indexNumb = 0

  for (index <- strBinMin.indices) {
    if (strBinMin(index) == '1') {
      val shift = strBinMin.length - 1 - index
      val x = intMax << shift
      numbs.update(indexNumb, x)
      indexNumb += 1

      println(s"at index: $index")
      println(s"x = ${String.format("%32s", intMax.toBinaryString).replace(' ', '0')} << $shift")
      println(s"x = ${String.format("%32s", x.toBinaryString).replace(' ', '0')}")
    }
  }

  println("\n\t-------------\n")

  var intXor = 0
  for (i <- numbs.indices) {
    println(s"intXor = ${String.format("%32s", intXor.toBinaryString).replace(' ', '0')}" +
      s"\n     xor ${String.format("%32s", numbs(i).toBinaryString).replace(' ', '0')}\n")
    intXor = intXor ^ numbs(i)
  }
  println(s"intXor = ${String.format("%32s", intXor.toBinaryString).replace(' ', '0')}")

  println("\n\t-------------\n")

  println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}")
  val moduloIntXor = 0x011b
  println(s"moduloIntXor = ${String.format("%32s", moduloIntXor.toBinaryString)}")

  while (intXor > moduloIntXor) {
    val shiftLeft = intXor.toBinaryString.length - moduloIntXor.toBinaryString.length
    println()
    println(s"shiftLeft: $shiftLeft")

    val moduloShift = moduloIntXor << shiftLeft
    println(s"moduloShift  = ${String.format("%32s",moduloIntXor.toBinaryString)} << $shiftLeft")
    println(s"moduloShift  = ${String.format("%32s", moduloShift.toBinaryString)}")
    println()
    println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}" +
      s"\n           xor ${String.format("%32s", moduloShift.toBinaryString)}")
    intXor = intXor ^ moduloShift
    println(s"intXor       = ${String.format("%32s", intXor.toBinaryString)}")
  }

  println("\n\t-------------\n")

  println(s"intXor = ${String.format("%10s",intXor.toBinaryString)}")
  println(s"intXor = 0b${String.format("%8s", intXor.toBinaryString).replace(' ','0')} 0x${String.format("%2s",intXor.toHexString).replace(' ', '0')}")

  intXor.toByte
}

def polynomialMultiplication(b1: Byte, b2: Byte): Byte = {

  val strBin1: String = (0xff & b1).toBinaryString
  val strBin2: String = (0xff & b2).toBinaryString

  println(s"b1        = ${String.format("%8s", (b1.toInt & 0xff).toBinaryString).replace(' ', '0')}")
  println(s"b2        = ${String.format("%8s", (b2.toInt & 0xff).toBinaryString).replace(' ', '0')}")

  val b1Bin = s"${ String.format("%8s", (b1.toInt & 0xff).toBinaryString).replace(' ', '0') }"
  val b2Bin = s"${ String.format("%8s", (b2.toInt & 0xff).toBinaryString).replace(' ', '0') }"
  val b1Hex = s"${ String.format("%2s", (b1.toInt & 0xff).toHexString).replace(' ', '0') }"
  val b2Hex = s"${ String.format("%2s", (b2.toInt & 0xff).toHexString).replace(' ', '0') }"

  if ((b1 & 0xff) > (b2 & 0xff)) {
    val byteResult = getPolynomial(b1.toInt & 0xff, strBin2)
    println(s"\n$b1Bin . $b2Bin = ${String.format("%8s", (byteResult & 0xff).toBinaryString).replace(' ', '0')}")
    println(s"$b1Hex . $b2Hex = ${String.format("%2s", (byteResult & 0xff).toHexString).replace(' ', '0')}")
    byteResult
  }
  else {
    val byteResult = getPolynomial(b2.toInt & 0xff, strBin1)
    println(s"\n$b1Bin . $b2Bin = ${String.format("%8s", (byteResult & 0xff).toBinaryString).replace(' ', '0')}")
    println(s"$b1Hex . $b2Hex = ${String.format("%2s", (byteResult & 0xff).toHexString).replace(' ', '0')}")
    byteResult
  }
}

object TryIt extends App {
  //    Aes.encodeDecodeTextWithKey("mm", "test")

  polynomialMultiplication(0xbf.toByte, 0x03.toByte)

  val test = 0xb3 ^ 0xda ^ 0x5d ^ 0x30
  println(s"0xb3 ^ 0xda ^ 0x5d ^ 0x30 = 0x${test.toHexString} = 0b${test.toBinaryString}")

}
