package fr.maxime.binandco
package tools.geometry.interfaces

trait MShape {
  val points: Array[MPoint]
}

object MShape {
  def of(points: Array[MPoint]): MShape = new MShape:
    override val points: Array[MPoint] = points
}
