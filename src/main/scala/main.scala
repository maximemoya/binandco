package fr.maxime.binandco

import tools.Tools.organizeTextIn64BytesBlocksWithLengthEndLine
import tools.{Sha256, Tools}

import java.nio.charset.Charset
import scala.util.control.Breaks.{break, breakable}

@main
def main(): Unit = {
  val hash = Sha256.hash("this is working")
  println(hash)
}
