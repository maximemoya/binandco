package fr.maxime.binandco
package tools.geometry.interfaces

import tools.geometry.interfaces.mshapes.MTriangle

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class GeometryTest extends AnyFlatSpec with should.Matchers {

  "Geometry_basic" should "MPoint_orderByX" in {
    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    val pointC = MPoint(4.0, 6.0)
    val pointD = MPoint(1.0, 1.0)
    MPoint.orderByX(Array(pointA, pointB, pointC, pointD)) should be(Array(pointD, pointA, pointB, pointC))
  }

  "Geometry_basic" should "MPoint_orderByY" in {
    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    val pointC = MPoint(4.0, 6.0)
    val pointD = MPoint(1.0, 1.0)
    MPoint.orderByY(Array(pointA, pointB, pointC, pointD)) should be(Array(pointD, pointA, pointB, pointC))
  }

  "Geometry_basic" should "MLine_getDirectionEquation" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)

    val line = MLine.default(pointA, pointB) // should make f(x) = 2.5 x - 1
    if (line.getDirectionEquation.isDefined){
      line.getDirectionEquation.get(0.0) should be(-1.0)
      line.getDirectionEquation.get(6.0) should be(8.0)
    }
    else {
      "NONE" should be(0)
    }

  }

  "Geometry_basic" should "MLine_getDirectionEquation_Vertical" in {

    val pointA = MPoint(2.0, 0.0)
    val pointB = MPoint(2.0, 5.0)

    val line = MLine.default(pointA, pointB)
    if (line.getDirectionEquation.isEmpty) {
      line.getDirection should be(DirectionMLine.UP)
    }
    else {
      "SOME" should be(0)
    }

    val lineReversed = MLine.default(pointB, pointA)
    if (lineReversed.getDirectionEquation.isEmpty) {
      lineReversed.getDirection should be(DirectionMLine.DOWN)
    }
    else {
      "SOME" should be(0)
    }

  }

  "Geometry_basic" should "MLine_getDirectionEquation_Horizontal" in {

    val pointA = MPoint(2.0, 5.0)
    val pointB = MPoint(4.0, 5.0)

    val line = MLine.default(pointA, pointB)
    if (line.getDirectionEquation.isDefined) {
      line.getDirection should be(DirectionMLine.RIGHT)
    }
    else {
      "NONE" should be(0)
    }

    val lineReversed = MLine.default(pointB, pointA)
    if (lineReversed.getDirectionEquation.isDefined) {
      lineReversed.getDirection should be(DirectionMLine.LEFT)
    }
    else {
      "NONE" should be(0)
    }

  }

  "Geometry_basic" should "MLine_IsBetweenX and Y" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    val line = MLine.default(pointA, pointB)

    val inPoint = MPoint(2.5, 3.5)
    line.isPointBetweenX(inPoint) should be(true)
    line.isPointBetweenY(inPoint) should be(true)

    val outPoint = MPoint(1.0, 6.0)
    line.isPointBetweenX(outPoint) should be(false)
    line.isPointBetweenY(outPoint) should be(false)

  }

}
