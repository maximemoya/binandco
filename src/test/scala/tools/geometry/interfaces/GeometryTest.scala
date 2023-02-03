package fr.maxime.binandco
package tools.geometry.interfaces

import tools.geometry.interfaces.mshapes.MTriangle

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class GeometryTest extends AnyFlatSpec with should.Matchers {

  "Geometry_basic" should "MLine_f(x)" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)

    val line = MLine.default(pointA, pointB) // should f(x) = 2.5 x - 1
    line.f(0.0) should be(-1.0)
    line.f(6.0) should be(8.0)

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

  "Geometry_basic" should "MLine_getStartPoint and EndPoint" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    
    val line = MLine.default(pointB, pointA)
    line.getStartPoint should be(pointA)
    line.getEndPoint should be(pointB)

  }

}
