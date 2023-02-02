package fr.maxime.binandco
package tools.geometry.interfaces

import tools.geometry.interfaces.MPoint

private trait MLine {

  def getStartPoint: MPoint

  def getEndPoint: MPoint

  /**
   * equation of the line as ' f (x) = y '
   *
   * @return A Double
   */
  def f: Double => Double

  def isPointBetweenX(point: MPoint): Boolean

  def isPointBetweenY(point: MPoint): Boolean

  def getMLineInfo: String

}

object MLine {
  def default(pointA: MPoint, pointB: MPoint): MLine = new MLine {

    override def getStartPoint: MPoint = {
      if (pointA.x < pointB.x) pointA
      else if (pointA.x == pointB.x) {
        if (pointA.y < pointB.y) pointA
        else pointB
      }
      else pointB
    }

    override def getEndPoint: MPoint = {
      if (pointA.x < pointB.x) pointB
      else if (pointA.x == pointB.x) {
        if (pointA.y < pointB.y) pointB
        else pointA
      }
      else pointA
    }

    private def calculateEquation(deltaX: Double, deltaY: Double): Double => Double = {
      if (deltaX != 0.0) {
        val coefficient: Double = if (deltaY == 0.0) 0.0 else deltaY / deltaX
        val extra = pointA.y - (coefficient * pointA.x)
        (x: Double) => coefficient * x + extra
      }
      else {
        (_: Double) => Double.MaxValue
      }
    }

    private val deltaX: Double = pointB.x - pointA.x
    private val deltaY: Double = pointB.y - pointA.y
    private val equation: Double => Double = calculateEquation(deltaX, deltaY)

    override def f: Double => Double = equation

    private def length(deltaX: Double, deltaY: Double): Double = Math.sqrt(deltaX * deltaX + deltaY * deltaY)

    override def getMLineInfo: String =
      s" MLine:" +
        s"\n\tEquation: Y = ${String.format("%.2f", equation(1) - equation(0))} X + ${String.format("%.2f", equation(0))}" +
        s"\n\tPointA: ( ${String.format("%.2f", pointA.x)} | ${String.format("%.2f", pointA.y)} )" +
        s"\n\tPointB: ( ${String.format("%.2f", pointB.x)} | ${String.format("%.2f", pointB.y)} )" +
        s"\n\tlength: ${String.format("%.2f", length(deltaX, deltaY))}"

    override def isPointBetweenX(point: MPoint): Boolean = {
      val startPointX = Math.min(pointA.x, pointB.x)
      val endPointX = Math.max(pointA.x, pointB.x)
      if (point.x >= startPointX && point.x <= endPointX) true
      else false
    }

    override def isPointBetweenY(point: MPoint): Boolean = {
      val startPointY = Math.min(pointA.y, pointB.y)
      val endPointY = Math.max(pointA.y, pointB.y)
      if (point.y >= startPointY && point.y <= endPointY) true
      else false
    }

  }

}
