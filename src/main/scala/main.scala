package fr.maxime.binandco

import tools.Tools
import tools.Tools.organizeTextIn64BytesBlocksWithLengthEndLine
import tools.sha.Sha256

import java.nio.charset.Charset
import scala.util.control.Breaks.{break, breakable}

@main
def main(): Unit = {
  val hash = Sha256.hash("this is working")
  println(hash)
}
