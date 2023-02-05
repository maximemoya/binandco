package fr.maxime.binandco
package tools.geometry.interfaces.mshapes

import tools.geometry.interfaces.{DirectionMLine, MLine, MPoint, MShape}

private trait MTriangle extends MShape {

  def isPointInside(point: MPoint): Boolean

}

object MTriangle {

  def default(pointA: MPoint, pointB: MPoint, pointC: MPoint): MTriangle = new MTriangle {
    private val points: Array[MPoint] = Array[MPoint](pointA, pointB, pointC)
    private val sides: Array[MLine] = {
      val lineA = MLine.default(pointA, pointB)
      val lineB = MLine.default(pointB, pointC)
      val lineC = MLine.default(pointC, pointA)
      Array[MLine](lineA, lineB, lineC)
    }
    private val maxRectPoint = MPoint(MPoint.getMaxX(points), MPoint.getMaxY(points))
    private val minRectPoint = MPoint(MPoint.getMinX(points), MPoint.getMinY(points))

    private def isPointInsideRectArea(point: MPoint): Boolean = {
      if (point.x >= minRectPoint.x
        && point.x <= maxRectPoint.x
        && point.y >= minRectPoint.y
        && point.y <= maxRectPoint.y) {
        true
      }
      else false
    }

    override def isPointInside(point: MPoint): Boolean = {

      if (!isPointInsideRectArea(point)) return false
      val pointsByXOrder = MPoint.orderByX(points)
      val line0 = MLine.default(pointsByXOrder(0),pointsByXOrder(1))
      val line1 = MLine.default(pointsByXOrder(0),pointsByXOrder(2))
      val line2 = MLine.default(pointsByXOrder(1),pointsByXOrder(2))

      false

    }

  }

}
