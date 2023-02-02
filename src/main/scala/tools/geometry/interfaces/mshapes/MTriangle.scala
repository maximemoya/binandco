package fr.maxime.binandco
package tools.geometry.interfaces.mshapes

import tools.geometry.interfaces.{MLine, MPoint, MShape}

private trait MTriangle extends MShape {

  override val points: Array[MPoint]
  override val sides: Array[MLine]

  def isPointInside(point: MPoint): Boolean

}

object MTriangle {

  def default(pointA: MPoint, pointB: MPoint, pointC: MPoint): MTriangle = new MTriangle {
    override val points: Array[MPoint] = Array[MPoint](pointA, pointB, pointC)
    override val sides: Array[MLine] = {
      val lineA = MLine.default(pointA, pointB)
      val lineB = MLine.default(pointB, pointC)
      val lineC = MLine.default(pointC, pointA)
      Array[MLine](lineA, lineB, lineC)
    }

    override def isPointInside(point: MPoint): Boolean = {
      val line0 = sides(0)
      val line1 = sides(1)
      val line2 = sides(2)

      //TODO: check condition with vertical line f(x)=infinite
      if (line0.isPointBetweenX(point) && line2.isPointBetweenX(point)) {
        // first part of triangle
        if (line0.getEndPoint.y < line2.getEndPoint.y) {
          // line0 below line2
          if (point.y >= line0.f(point.x) && point.y <= line2.f(point.x)) true
          else false
        }
        else {
          // line2 below line0
          if (point.y >= line2.f(point.x) && point.y <= line0.f(point.x)) true
          else false
        }
      }
      else if (line1.isPointBetweenX(point) && line2.isPointBetweenX(point)) {
        // second part of triangle
        if (line1.getStartPoint.y < line2.getStartPoint.y) {
          // line1 below line2
          if (point.y >= line1.f(point.x) && point.y <= line2.f(point.x)) true
          else false
        }
        else {
          // line2 below line1
          if (point.y >= line2.f(point.x) && point.y <= line1.f(point.x)) true
          else false
        }
      }
      else false
    }
  }

}
