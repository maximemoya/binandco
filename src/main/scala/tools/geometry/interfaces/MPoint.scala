package fr.maxime.binandco
package tools.geometry.interfaces

trait MPoint {
  val x: Double
  val y: Double
}

object MPoint {
  def apply(_x: Double, _y: Double): MPoint = new MPoint {
    override val x: Double = _x
    override val y: Double = _y
  }
}
