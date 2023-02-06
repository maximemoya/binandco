package fr.maxime.binandco
package tools.geometry.interfaces.mshapes

import tools.geometry.interfaces.{MLine, MPoint, MShape}

private trait MTriangle extends MShape {

  def isPointInside(point: MPoint): Boolean

}

object MTriangle {

  def default(pointA: MPoint, pointB: MPoint, pointC: MPoint): MTriangle = new MTriangle {

    private val points: Array[MPoint] = Array[MPoint](pointA, pointB, pointC)
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
      if (point.equal(points)) return true

      val eq1 = (points(0).x - point.x) * (points(1).y - point.y) - (points(0).y - point.y) * (points(1).x - point.x)
      val eq2 = (points(1).x - point.x) * (points(2).y - point.y) - (points(1).y - point.y) * (points(2).x - point.x)
      val eq3 = (points(2).x - point.x) * (points(0).y - point.y) - (points(2).y - point.y) * (points(0).x - point.x)

      if (eq1.sign == 0.0 || eq1.sign == -0.0) {
        if (eq2.sign == 0.0 || eq2.sign == -0.0) {
          true
        }
        else {
          if (eq2.sign == eq3.sign) true else false
        }
      }
      else if (eq2.sign == 0.0 || eq2.sign == -0.0) {
        if (eq3.sign == 0.0 || eq3.sign == -0.0) {
          true
        }
        else {
          if (eq1.sign == eq3.sign) true else false
        }
      }
      else if (eq3.sign == 0.0 || eq3.sign == -0.0) {
        if (eq1.sign == eq2.sign) true else false
      }
      else {
        if (eq1.sign == eq2.sign && eq1.sign == eq3.sign) true else false
      }

    }

  }

}
