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

      val line0 = sides(0)
      val line1 = sides(1)
      val line2 = sides(2)

      line0.getDirection match {
        case DirectionMLine.DOWN =>
          if (line0.getDirectionEquation.isEmpty) {
            if (line1.getDirection == DirectionMLine.LEFT) {
              if (
                point.x <= line0.getStartPoint.x
                  && line1.getDirectionEquation.isDefined
                  && point.y >= line1.getDirectionEquation.get(point.x)
                  && line2.getDirectionEquation.isDefined
                  && point.y <= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else if (line1.getDirection == DirectionMLine.RIGHT) {
              if (
                point.x >= line0.getStartPoint.x
                  && line1.getDirectionEquation.isDefined
                  && point.y >= line1.getDirectionEquation.get(point.x)
                  && line2.getDirectionEquation.isDefined
                  && point.y <= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else {
              if (line0.getEndPoint.x >= line1.getEndPoint.x) {
                //line1 is at left side
                if (
                  point.x <= line0.getStartPoint.x
                    && line1.getDirectionEquation.isDefined
                    && point.y >= line1.getDirectionEquation.get(point.x)
                    && line2.getDirectionEquation.isDefined
                    && point.y <= line2.getDirectionEquation.get(point.x)
                ) {
                  true
                }
                else false
              }
              else {
                //line1 is at right side
                if (
                  point.x >= line0.getStartPoint.x
                    && line1.getDirectionEquation.isDefined
                    && point.y >= line1.getDirectionEquation.get(point.x)
                    && line2.getDirectionEquation.isDefined
                    && point.y <= line2.getDirectionEquation.get(point.x)
                ) {
                  true
                }
                else false
              }
            }
          }
          else {
            //TODO:
            false
          }
        case DirectionMLine.UP =>
          if (line0.getDirectionEquation.isEmpty) {
            if (line1.getDirection == DirectionMLine.LEFT) {
              if (
                point.x <= line0.getStartPoint.x
                  && line1.getDirectionEquation.isDefined
                  && point.y <= line1.getDirectionEquation.get(point.x)
                  && line2.getDirectionEquation.isDefined
                  && point.y >= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else if (line1.getDirection == DirectionMLine.RIGHT) {
              if (
                point.x >= line0.getStartPoint.x
                  && line1.getDirectionEquation.isDefined
                  && point.y <= line1.getDirectionEquation.get(point.x)
                  && line2.getDirectionEquation.isDefined
                  && point.y >= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else {
              if (line0.getEndPoint.x >= line1.getEndPoint.x) {
                //line1 is at left side
                if (
                  point.x <= line0.getStartPoint.x
                    && line1.getDirectionEquation.isDefined
                    && point.y <= line1.getDirectionEquation.get(point.x)
                    && line2.getDirectionEquation.isDefined
                    && point.y >= line2.getDirectionEquation.get(point.x)
                ) {
                  true
                }
                else false
              }
              else {
                //line1 is at right side
                if (
                  point.x >= line0.getStartPoint.x
                    && line1.getDirectionEquation.isDefined
                    && point.y <= line1.getDirectionEquation.get(point.x)
                    && line2.getDirectionEquation.isDefined
                    && point.y >= line2.getDirectionEquation.get(point.x)
                ) {
                  true
                }
                else false
              }
            }
          }
          else {
            //TODO:
            false
          }
        case DirectionMLine.LEFT =>
          if (line1.getDirection == DirectionMLine.UP) {
            if (
              line0.getDirectionEquation.isDefined
                && point.y >= line0.getDirectionEquation.get(point.x)
                && line1.getDirectionEquation.isDefined
                && point.y <= line1.getDirectionEquation.get(point.x)
                && line2.getDirectionEquation.isDefined
                && point.y >= line2.getDirectionEquation.get(point.x)
            ) {
              true
            }
            else false
          }
          else {
            if (
              line0.getDirectionEquation.isDefined
                && point.y <= line0.getDirectionEquation.get(point.x)
                && line1.getDirectionEquation.isDefined
                && point.y >= line1.getDirectionEquation.get(point.x)
                && line2.getDirectionEquation.isDefined
                && point.y <= line2.getDirectionEquation.get(point.x)
            ) {
              true
            }
            else false
          }
        case DirectionMLine.RIGHT =>
          if (line1.getDirection == DirectionMLine.UP) {
            if(line1.getDirectionEquation.isEmpty){
              if (
                line0.getDirectionEquation.isDefined
                  && point.y >= line0.getDirectionEquation.get(point.x)
                  && point.x <= line1.getStartPoint.x
                  && line2.getDirectionEquation.isDefined
                  && point.y <= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else if (
              line0.getDirectionEquation.isDefined
                && point.y >= line0.getDirectionEquation.get(point.x)
                && line1.getDirectionEquation.isDefined
                && point.y >= line1.getDirectionEquation.get(point.x)
                && line2.getDirectionEquation.isDefined
                && point.y <= line2.getDirectionEquation.get(point.x)
            ) {
              true
            }
            else false
          }
          else {
            if (line1.getDirectionEquation.isEmpty) {
              if (
                line0.getDirectionEquation.isDefined
                  && point.y <= line0.getDirectionEquation.get(point.x)
                  && point.x <= line1.getStartPoint.x
                  && line2.getDirectionEquation.isDefined
                  && point.y >= line2.getDirectionEquation.get(point.x)
              ) {
                true
              }
              else false
            }
            else if (
              line0.getDirectionEquation.isDefined
                && point.y <= line0.getDirectionEquation.get(point.x)
                && line1.getDirectionEquation.isDefined
                && point.y <= line1.getDirectionEquation.get(point.x)
                && line2.getDirectionEquation.isDefined
                && point.y >= line2.getDirectionEquation.get(point.x)
            ) {
              true
            }
            else false
          }
      }

    }

    //    override def isPointInside(point: MPoint): Boolean = {
    //
    //      if (!isPointInsideRectArea(point)) return false
    //
    //      val line0 = sides(0)
    //      val line1 = sides(1)
    //      val line2 = sides(2)
    //
    //      if (line0.isPointBetweenX(point) && line2.isPointBetweenX(point)) {
    //        if (line0.getEndPoint.y < line2.getEndPoint.y) {
    //          // line0 below line2
    //          if (point.y >= line0.f(point.x) && point.y <= line2.f(point.x)) true
    //          else false
    //        }
    //        else if (line0.getEndPoint.y > line2.getEndPoint.y) {
    //          // line2 below line0
    //          if (point.y >= line2.f(point.x) && point.y <= line0.f(point.x)) true
    //          else false
    //        }
    //        else {
    //          // line0.getEndPoint.y == line2.getEndPoint.y
    //          if (line0.getStartPoint.y < line2.getStartPoint.y) {
    //            // line0 below line2
    //            if (point.y >= line0.f(point.x) && point.y <= line2.f(point.x)) true
    //            else false
    //          }
    //          else {
    //            // line2 below line0
    //            if (point.y >= line2.f(point.x) && point.y <= line0.f(point.x)) true
    //            else false
    //          }
    //        }
    //      }
    //      else if (line1.isPointBetweenX(point) && line2.isPointBetweenX(point)) {
    //        if (line1.getEndPoint.y < line2.getEndPoint.y) {
    //          // line1 below line2
    //          if (point.y >= line1.f(point.x) && point.y <= line2.f(point.x)) true
    //          else false
    //        }
    //        else if (line1.getEndPoint.y > line2.getEndPoint.y) {
    //          // line2 below line1
    //          if (point.y >= line2.f(point.x) && point.y <= line1.f(point.x)) true
    //          else false
    //        }
    //        else {
    //          // line1.getEndPoint.y == line2.getEndPoint.y
    //          if (line1.getStartPoint.y < line2.getStartPoint.y) {
    //            // line1 below line2
    //            if (point.y >= line1.f(point.x) && point.y <= line2.f(point.x)) true
    //            else false
    //          }
    //          else {
    //            // line2 below line1
    //            if (point.y >= line2.f(point.x) && point.y <= line1.f(point.x)) true
    //            else false
    //          }
    //        }
    //      }
    //      else if (line0.isPointBetweenX(point) && line1.isPointBetweenX(point)) {
    //        if (line0.getEndPoint.y < line1.getEndPoint.y) {
    //          // line0 below line1
    //          if (point.y >= line0.f(point.x) && point.y <= line1.f(point.x)) true
    //          else false
    //        }
    //        else if (line0.getEndPoint.y > line1.getEndPoint.y) {
    //          // line1 below line0
    //          if (point.y >= line1.f(point.x) && point.y <= line0.f(point.x)) true
    //          else false
    //        }
    //        else {
    //          // line0.getEndPoint.y == line1.getEndPoint.y
    //          if (line0.getStartPoint.y < line1.getStartPoint.y) {
    //            // line0 below line1
    //            if (point.y >= line0.f(point.x) && point.y <= line1.f(point.x)) true
    //            else false
    //          }
    //          else {
    //            // line1 below line0
    //            if (point.y >= line1.f(point.x) && point.y <= line0.f(point.x)) true
    //            else false
    //          }
    //        }
    //      }
    //      else false
    //    }

  }

}
