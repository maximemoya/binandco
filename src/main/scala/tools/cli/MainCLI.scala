package fr.maxime.binandco
package tools.cli

import scala.annotation.tailrec
import scala.io.StdIn.*
import scala.io.Source._
import java.io._

object MainCLI extends App {

  println("welcome to CLI")
  input("/main")

}

@tailrec
def input(txt: String): Unit = {

  if (txt == "/main") {
    println("command < exit > or < quit > to shutdown the CLI")
    println("command < help > or < ? > to get available commands")
  }
  else if (txt == "help" || txt == "?") {
    println(" =>")
    println("\tcommand < exit > or < quit > to shutdown the CLI")
    println("\tcommand < help > or < ? > to get available commands")
  }
  else if (txt == "write") {
    writeFile("test.txt", "un test d'écriture")
    println(s" => 'un test d'écriture' written in file 'test.txt'")
  }
  else if (txt == "read") {
    val fileTxt = readFile("test.txt")
    println(s" => '$fileTxt' read from the file 'test.txt'")
  }
  else {
    println(s" /!\\ unknown command: $txt")
  }

  print("command: ")
  val lineTxt = readLine()
  if (lineTxt == "quit" || lineTxt == "exit") {
    println("GoodBye :)")
    Thread.sleep(300)
    sys.exit()
  }
  else {
    input(lineTxt)
  }

}

/**
 * write a `Seq[String]` to the `filename`.
 */
def writeFile(filename: String, lines: Seq[String]): Unit = {
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  for (line <- lines) {
    bw.write(line)
  }
  bw.close()
}

/**
 * write a `String` to the `filename`.
 */
def writeFile(filename: String, s: String): Unit = {
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(s)
  bw.close()
}

/**
 * read file from `filename`.
 */
def readFile(filename: String): String = {
  try {
    val source = fromFile(filename)
    val txt = source.mkString
    source.close
    txt
  }
  catch {
    case _ => "empty"
  }
}

