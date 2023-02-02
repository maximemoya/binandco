package fr.maxime.binandco
package tools.geometry.interfaces

import tools.geometry.interfaces.mshapes.MTriangle

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class GeometryTest extends AnyFlatSpec with should.Matchers {

  "Geometry_basic" should "lines_f(x)" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    // f(x) = 2.5 x - 1
    val line = MLine.default(pointA, pointB)

    line.f(0.0) should be(-1.0)
    line.f(6.0) should be(8.0)

  }

  "Geometry_basic" should "lines_IsBetweenX and Y" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    val line = MLine.default(pointA, pointB)

    val inPoint = MPoint(2.5, 3.5)
    val outPoint = MPoint(1.0, 6.0)

    line.isPointBetweenX(inPoint) should be(true)
    line.isPointBetweenY(inPoint) should be(true)

    line.isPointBetweenX(outPoint) should be(false)
    line.isPointBetweenY(outPoint) should be(false)

  }

  "Geometry_basic" should "lines_getStartPoint and EndPoint" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 5.0)
    val line = MLine.default(pointB, pointA)

    line.getStartPoint should be(pointA)
    line.getEndPoint should be(pointB)

  }

  "Geometry_MShapes" should "MTriangle isPointInside" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 2.0)
    val pointC = MPoint(4.0, 4.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePoint = MPoint(2.5, 3.0)
    val insidePoint = MPoint(2.5, 2.3)

    triangle.isPointInside(outsidePoint) should be(false)
    triangle.isPointInside(insidePoint) should be(true)

  }

}
