package fr.maxime.binandco
package tools.cli

trait MNone {}

private class MEffect[E, V](_error: E, _value: V) {
  private val value: V = _value
  private val error: E = _error

  def resolveEffectOrFail: V = {
    if (!this.error.isInstanceOf[MNone]) {
      throw new Error(this.error.toString)
    }
    else {
      this.value
    }
  }

  def convertErrorToValue: MEffect[MNone, E] = {
    MEffect.succeed(this.error)
  }

  def convertValueToError: MEffect[V, MNone] = {
    MEffect.failed(this.value)
  }

}

object MEffect {

  def succeed[V](value: V): MEffect[MNone, V] = {
    new MEffect[MNone, V](new MNone {}, value)
  }

  def failed[E](error: E): MEffect[E, MNone] = {
    new MEffect[E, MNone](error, new MNone {})
  }

}

object TestMEffect extends App {

  val a = MEffect.succeed("hello")
  val b = a.resolveEffectOrFail
  println(s"effect value: $b")

  val c = MEffect.failed("MyError")
  val d = c.convertErrorToValue
  println(s"effect error: ${d.resolveEffectOrFail}")

  val e = a.convertValueToError
  println(s"effect value to error: ${e.resolveEffectOrFail}")

}
