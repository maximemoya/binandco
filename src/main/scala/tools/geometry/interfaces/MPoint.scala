package fr.maxime.binandco
package tools.geometry.interfaces

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private trait MPoint {
  val x: Double
  val y: Double

  def equal(points: Array[MPoint]): Boolean = {
    for (point <- points) {
      if (point.x == x && point.y == y) return true
    }
    false
  }

}

object MPoint {
  def apply(_x: Double, _y: Double): MPoint = new MPoint {
    override val x: Double = _x
    override val y: Double = _y
  }

  def orderByX(points: Array[MPoint]): Array[MPoint] = {
    points.sortBy(point => point.x)
  }

  def orderByY(points: Array[MPoint]): Array[MPoint] = {
    points.sortBy(point => point.y)
  }

  def getMaxX(points: Array[MPoint]): Double = {
    var result = points(0).x
    for (point <- points) {
      if (point.x > result) result = point.x
    }
    result
  }

  def getMinX(points: Array[MPoint]): Double = {
    var result = points(0).x
    for (point <- points) {
      if (point.x < result) result = point.x
    }
    result
  }

  def getMaxY(points: Array[MPoint]): Double = {
    var result = points(0).y
    for (point <- points) {
      if (point.y > result) result = point.y
    }
    result
  }

  def getMinY(points: Array[MPoint]): Double = {
    var result = points(0).y
    for (point <- points) {
      if (point.y < result) result = point.y
    }
    result
  }

}
