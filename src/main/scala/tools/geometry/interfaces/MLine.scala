package fr.maxime.binandco
package tools.geometry.interfaces

import tools.geometry.interfaces.MPoint

private trait MLine {

  def getPointA: MPoint

  def getPointB: MPoint

  /**
   * equation of the line as ' f (x) = y '
   * @return A Double
   */
  def f: Double => Double

  def getMLineInfo: String

}

object MLine {
  def default(pointA: MPoint, pointB: MPoint): MLine = new MLine {

    override def getPointA: MPoint = pointA

    override def getPointB: MPoint = pointB

    override def f: Double => Double = equation

    override def getMLineInfo: String =
      s" MLine:" +
        s"\n\tEquation: Y = ${String.format("%.2f", equation(1) - equation(0))} X + ${String.format("%.2f", equation(0))}" +
        s"\n\tPointA: ( ${String.format("%.2f", pointA.x)} | ${String.format("%.2f", pointA.y)} )" +
        s"\n\tPointB: ( ${String.format("%.2f", pointB.x)} | ${String.format("%.2f", pointB.y)} )" +
        s"\n\tlength: ${String.format("%.2f", length(deltaX, deltaY))}"

    private val deltaX: Double = pointB.x - pointA.x
    private val deltaY: Double = pointB.y - pointA.y
    private val equation: Double => Double = calculateEquation(deltaX, deltaY)

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

    private def length(deltaX: Double, deltaY: Double): Double = Math.sqrt(deltaX * deltaX + deltaY * deltaY)

  }

  def light(pointA: MPoint, pointB: MPoint): MLine = new MLine {

    override def getPointA: MPoint = pointA

    override def getPointB: MPoint = pointB
    
    override def f: Double => Double = equation

    override def getMLineInfo: String =
      s" MLine:" +
        s"\n\tEquation: Y = ${String.format("%.2f", equation(1) - equation(0))} X + ${String.format("%.2f", equation(0))}"

    private val deltaX: Double = pointB.x - pointA.x
    private val deltaY: Double = pointB.y - pointA.y
    private val equation: Double => Double = calculateEquation(deltaX, deltaY)

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
    
  }

}
