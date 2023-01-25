package fr.maxime.binandco
package tools.geometry.interfaces.mshapes

import tools.geometry.interfaces.{MLine, MPoint, MShape}

trait MTriangle extends MShape {

  override val points: Array[MPoint]
  override val sides: Array[MLine]

}
