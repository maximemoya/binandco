package fr.maxime.binandco
package tools.exercises

trait Summable[T] {
  def sumElements(list: List[T]): T
}

implicit object IntSummable extends Summable[Int] {
  override def sumElements(list: List[Int]): Int = list.sum
}

implicit object StringSummable extends Summable[String] {
  override def sumElements(list: List[String]): String = list.mkString("")
}

def processMyList[T](list: List[T])(implicit summable: Summable[T]): T = {
  summable.sumElements(list)
}

private object ImplicitTest extends App {
  println(processMyList(List(1, 2, 3)))
  println(processMyList(List("a", "b", "c")))
}
