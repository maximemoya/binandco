package fr.maxime.binandco
package tools.geometry

import tools.geometry.interfaces.{MLine, MPoint, MShape}

object Geometry extends App {

  private val mShape = MShape.default(Array(MPoint(0,1),MPoint(1,0),MPoint(0,0)))
  println(mShape.sides.length)

}
