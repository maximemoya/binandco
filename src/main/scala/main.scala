package fr.maxime.binandco

import java.nio.charset.Charset

@main
def main(): Unit = {
  val text = "Hello world!"
  println(text)
  printBinariesInfoAboutText(text)
}

def printBinariesInfoAboutText(text:String): Unit ={

  val bytes = text.getBytes(Charset.forName("UTF-8"))
  for(index <- bytes.indices){
    println(s"'${text.apply(index)}'= 0x${bytes.apply(index).toInt.toHexString} = 0b${bytes.apply(index).toInt.toBinaryString}")
  }

}
