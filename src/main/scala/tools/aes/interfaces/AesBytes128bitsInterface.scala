package fr.maxime.binandco
package tools.aes.interfaces

import tools.aes.interfaces.Table16x16
import tools.aes.interfaces.implementations.AesBytes128bitsImplementationRegular

trait AesBytes128bitsInterface {
  val bytes128: Bytes128

  def printBytes(): Unit = {
    bytes128.printBytes128()
  }

  /**
   * should transform this.bytes128
   *
   * @param keyExpansion Array of Array[Int]
   * @param round        A RoundIndex by Int
   * @return should be this
   */
  def addRoundKey(keyExpansion: KeyExpansion128bits, round: Int): AesBytes128bitsInterface

  /**
   * should transform this.bytes128
   *
   * @param tableEncode A Table16x16
   * @return should be this
   */
  def subBytes(tableEncode: Table16x16): AesBytes128bitsInterface

  /**
   * should transform this.bytes128
   *
   * @return should be this
   */
  def shiftRowsEncode(): AesBytes128bitsInterface

  /**
   * should transform this.bytes128
   *
   * @return should be this
   */
  def shiftRowsDecode(): AesBytes128bitsInterface

  /**
   * should transform this.bytes128
   *
   * @param galoisFieldBox A Bytes128
   * @return should be this
   */
  def mixColumns(galoisFieldBox: Bytes128): AesBytes128bitsInterface

}
