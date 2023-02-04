package fr.maxime.binandco
package tools.geometry.interfaces.mshapes

import tools.geometry.interfaces.MPoint
import tools.geometry.interfaces.mshapes.MTriangle

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class MTriangleTest extends AnyFlatSpec with should.Matchers {

  "Geometry_MTriangle" should "isPointInside_T1_RIGHT" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 2.0)
    val pointC = MPoint(4.5, 4.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(4.0, 4.1)
    val outsidePointLeft = MPoint(2.5, 3.0)
    val outsidePointDown = MPoint(3.0, 1.9)
    val outsidePointRight = MPoint(4.5, 3.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(4.5, 4.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(3.0, 2.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.5, 3.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T1_RIGHT_Mirror" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 2.0)
    val pointC = MPoint(5.0, 0.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(3.0, 2.1)
    val outsidePointLeft = MPoint(3.5, 0.5)
    val outsidePointDown = MPoint(5.0, -0.1)
    val outsidePointRight = MPoint(4.6, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(3.0, 2.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(5.0, 0.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T1_LEFT" in {

    val pointA = MPoint(4.0, 2.0)
    val pointB = MPoint(2.0, 2.0)
    val pointC = MPoint(4.5, 4.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(4.0, 4.1)
    val outsidePointLeft = MPoint(2.5, 3.0)
    val outsidePointDown = MPoint(3.0, 1.9)
    val outsidePointRight = MPoint(4.5, 3.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(4.5, 4.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(3.0, 2.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.5, 3.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T1_LEFT_Mirror" in {

    val pointA = MPoint(4.0, 2.0)
    val pointB = MPoint(2.0, 2.0)
    val pointC = MPoint(5.0, 0.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(3.0, 2.1)
    val outsidePointLeft = MPoint(3.5, 0.5)
    val outsidePointDown = MPoint(5.0, -0.1)
    val outsidePointRight = MPoint(4.6, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(3.0, 2.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(5.0, 0.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T2_DOWN_THEN_UP" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(1.0, 1.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(1.5, 1.6)
    val outsidePointLeft = MPoint(0.9, 1.0)
    val outsidePointDown = MPoint(1.5, 0.4)
    val outsidePointRight = MPoint(2.1, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(1.0, 1.0)
    val insidePointDown = MPoint(2.0, 0.0)
    val insidePointRight = MPoint(2.0, 1.0)
    val insidePointCenter = MPoint(1.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T2_DOWN_THEN_UP_Mirror" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(3.0, 1.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(2.5, 1.6)
    val outsidePointLeft = MPoint(1.9, 1.0)
    val outsidePointDown = MPoint(2.5, 0.4)
    val outsidePointRight = MPoint(3.1, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(2.0, 1.0)
    val insidePointDown = MPoint(2.0, 0.0)
    val insidePointRight = MPoint(3.0, 1.0)
    val insidePointCenter = MPoint(2.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T3_DOWN_THEN_DOWN" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(4.0, -1.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(2.0, 2.1)
    val outsidePointLeft = MPoint(1.9, 1.0)
    val outsidePointDown = MPoint(3.0, -0.6)
    val outsidePointRight = MPoint(3.0, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(2.0, 0.0)
    val insidePointDown = MPoint(3.0, -0.5)
    val insidePointRight = MPoint(3.0, 0.5)
    val insidePointCenter = MPoint(2.5, 0.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T3_DOWN_THEN_DOWN_Mirror" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(0.0, -1.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(2.0, 2.1)
    val outsidePointLeft = MPoint(0.9, 0.5)
    val outsidePointDown = MPoint(1.0, -0.6)
    val outsidePointRight = MPoint(2.1, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(1.0, 0.5)
    val insidePointDown = MPoint(1.0, -0.5)
    val insidePointRight = MPoint(2.0, 0.5)
    val insidePointCenter = MPoint(1.5, 0.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T4_DOWN_THEN_LEFT" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(0.0, 0.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(2.0, 2.1)
    val outsidePointLeft = MPoint(1.0, 1.1)
    val outsidePointDown = MPoint(1.0, -0.1)
    val outsidePointRight = MPoint(2.1, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(0.0, 0.0)
    val insidePointDown = MPoint(2.0, 0.0)
    val insidePointRight = MPoint(2.0, 1.0)
    val insidePointCenter = MPoint(1.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_T4_DOWN_THEN_RIGHT" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(2.0, 0.0)
    val pointC = MPoint(4.0, 0.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(2.0, 2.1)
    val outsidePointLeft = MPoint(1.9, 1.0)
    val outsidePointDown = MPoint(3.0, -0.1)
    val outsidePointRight = MPoint(3.1, 1.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(2.0, 2.0)
    val insidePointLeft = MPoint(2.0, 0.0)
    val insidePointDown = MPoint(4.0, 0.0)
    val insidePointRight = MPoint(3.0, 1.0)
    val insidePointCenter = MPoint(2.5, 1.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  // ---

  /*
  "Geometry_MTriangle" should "isPointInside_Normal_RIGHT" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 2.0)
    val pointC = MPoint(4.0, 4.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(4.0, 4.1)
    val outsidePointLeft = MPoint(2.5, 3.0)
    val outsidePointDown = MPoint(3.0, 1.9)
    val outsidePointRight = MPoint(4.1, 3.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(4.0, 4.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(3.0, 2.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.0, 2.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_Normal_UP_RIGHT" in {

    val pointA = MPoint(2.0, 2.0)
    val pointB = MPoint(4.0, 4.0)
    val pointC = MPoint(4.0, 2.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(4.0, 4.1)
    val outsidePointLeft = MPoint(2.5, 3.0)
    val outsidePointDown = MPoint(3.0, 1.9)
    val outsidePointRight = MPoint(4.1, 3.0)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(4.0, 4.0)
    val insidePointLeft = MPoint(2.0, 2.0)
    val insidePointDown = MPoint(3.0, 2.0)
    val insidePointRight = MPoint(4.0, 2.0)
    val insidePointCenter = MPoint(3.0, 2.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_Normal_Mirror_LEFT" in {

    val pointA = MPoint(4.0, 4.0)
    val pointB = MPoint(2.0, 4.0)
    val pointC = MPoint(2.0, 2.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(3.0, 4.1)
    val outsidePointLeft = MPoint(1.9, 3.0)
    val outsidePointDown = MPoint(2.0, 1.9)
    val outsidePointRight = MPoint(3.0, 2.9)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(3.0, 4.0)
    val insidePointLeft = MPoint(2.0, 3.0)
    val insidePointDown = MPoint(2.0, 2.0)
    val insidePointRight = MPoint(3.0, 3.0)
    val insidePointCenter = MPoint(3.0, 3.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_Normal_Mirror_DOWN_LEFT" in {

    val pointA = MPoint(4.0, 4.0)
    val pointB = MPoint(2.0, 2.0)
    val pointC = MPoint(2.0, 4.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(3.0, 4.1)
    val outsidePointLeft = MPoint(1.9, 3.0)
    val outsidePointDown = MPoint(2.0, 1.9)
    val outsidePointRight = MPoint(3.0, 2.9)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(3.0, 4.0)
    val insidePointLeft = MPoint(2.0, 3.0)
    val insidePointDown = MPoint(2.0, 2.0)
    val insidePointRight = MPoint(3.0, 3.0)
    val insidePointCenter = MPoint(3.0, 3.5)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)
    triangle.isPointInside(insidePointCenter) should be(true)

  }

  "Geometry_MTriangle" should "isPointInside_Extra" in {

    val pointA = MPoint(0.0, 2.0)
    val pointB = MPoint(-5.0, -5.0)
    val pointC = MPoint(5.0, 0.0)
    val triangle = MTriangle.default(pointA, pointB, pointC)

    val outsidePointUp = MPoint(0.0, 2.1)
    val outsidePointLeft = MPoint(-3.0, -1.5)
    val outsidePointDown = MPoint(0.0, -3.0)
    val outsidePointRight = MPoint(2.5, 1.1)

    triangle.isPointInside(outsidePointUp) should be(false)
    triangle.isPointInside(outsidePointLeft) should be(false)
    triangle.isPointInside(outsidePointDown) should be(false)
    triangle.isPointInside(outsidePointRight) should be(false)

    val insidePointUp = MPoint(0.0, 1.0)
    val insidePointLeft = MPoint(-3.0, -3.0)
    val insidePointDown = MPoint(0.0, -2.0)
    val insidePointRight = MPoint(3.0, 0.0)

    triangle.isPointInside(insidePointUp) should be(true)
    triangle.isPointInside(insidePointLeft) should be(true)
    triangle.isPointInside(insidePointDown) should be(true)
    triangle.isPointInside(insidePointRight) should be(true)

  }
  */

}
