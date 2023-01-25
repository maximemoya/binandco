package fr.maxime.binandco
package tools.geometry.interfaces

trait MShape {
  val points: Array[MPoint]
  val sides: Array[MLine]
}

object MShape {
  def default(_points: Array[MPoint]): MShape = new MShape:
    override val points: Array[MPoint] = _points
    override val sides: Array[MLine] = new Array[MLine](sidesCount)

    private def sidesCount: Int =
        if (points.length < 2) 0
        else if (points.length < 3) 1
        else points.length

}
