package fr.maxime.binandco
package tools.cli.models

trait MGenericError {
  def getFullTag: String = "MGenericError"

  def getTag: String = classOf[MGenericError].getSimpleName
}

trait TestErrorA extends MGenericError {
  override def getFullTag: String = "MGenericError(TestErrorA)"

  override def getTag: String = classOf[TestErrorA].getSimpleName
}

case class TestErrorA1() extends TestErrorA {
  override def getFullTag: String = "MGenericError(TestErrorA(TestErrorA1))"

  override def getTag: String = classOf[TestErrorA1].getSimpleName
}

trait TestErrorA2 extends TestErrorA {
  override def getFullTag: String = "MGenericError(TestErrorA(TestErrorA2))"

  override def getTag: String = classOf[TestErrorA2].getSimpleName
}

trait TestErrorB extends MGenericError {
  override def getFullTag: String = "MGenericError(TestErrorA(TestErrorB))"

  override def getTag: String = classOf[TestErrorB].getSimpleName
}

trait TestErrorB1 extends TestErrorB {
  override def getFullTag: String = "MGenericError(TestErrorA(TestErrorB1))"

  override def getTag: String = classOf[TestErrorB1].getSimpleName
}

trait TestErrorB2 extends TestErrorB {
  override def getFullTag: String = "MGenericError(TestErrorA(TestErrorB2))"

  override def getTag: String = classOf[TestErrorB2].getSimpleName
}

def tryIt(n: Int): TestErrorA = {
  if (n > 100) {
    new TestErrorA1 {}
  }
  else {
    new TestErrorA2 {}
  }
}

object MGenericError extends App {

  val error = tryIt(5)
  println(s"error: ${error.getFullTag}")
  println(s"is ${error.getTag} ?= ${classOf[TestErrorA1].getSimpleName}")
  if (error.getTag == classOf[TestErrorA1].getSimpleName) {
    println(s" => yes it is a classOf[TestErrorA1]")
  }
  else {
    println(" => no")
  }

}
