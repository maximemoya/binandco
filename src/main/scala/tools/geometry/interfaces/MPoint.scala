package fr.maxime.binandco
package tools.geometry.interfaces

trait MPoint {
  val x: Int
  val y: Int
}

object MPoint {
  def of(x: Int, y: Int): MPoint = new MPoint {
    override val x: Int = x
    override val y: Int = y
  }
}
