package fr.maxime.binandco
package tools.sha

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ShaTest extends AnyFlatSpec with should.Matchers {

  "Sha256" should "Hash" in {

    val hash = Sha256.hash("Ça marche ! it works ! 有用 !")
    hash should be("4e37fad5fed2ec51ab28853a17e58da29d64dcca3bb6bf354439b16f7f64e749")

  }

}