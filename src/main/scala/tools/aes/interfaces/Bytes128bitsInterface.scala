package fr.maxime.binandco
package tools.aes.interfaces

import tools.aes.interfaces.Table16x16
import tools.aes.interfaces.implementations.Bytes128bitsImplementationMM

trait Bytes128bitsInterface {
  val bytes128: Bytes128

  def addRoundKey(bytes: Bytes128, keyExpansion: Array[Array[Int]], round: Int): Bytes128
  
  def subBytes(bytes: Bytes128, tableEncode: Table16x16): Bytes128
  
  def shiftRowsEncode(bytes: Bytes128): Bytes128
  
  def shiftRowsDecode(bytes: Bytes128): Bytes128

  def mixColumns(bytes: Bytes128, galoisFieldBox: Bytes128): Bytes128

}
