package fr.maxime.binandco
package tools.aes.interfaces

trait AesBlocksBytes128bitsInterface {

  val blocks: Array[AesBytes128bitsInterface]

  val keyExpansionEncode: KeyExpansion128bits
  //TODO: make decode key expansion method
  //  val keyExpansionDecode: KeyExpansion128bits
  val tableEncode: Table16x16
  val tableDecode: Table16x16
  val galoisFieldEncodeBox: Bytes128
  val galoisFieldDecodeBox: Bytes128

  def printBlocks(): Unit = {
    for (i <- blocks.indices) {
      println(s"index: $i")
      blocks(i).printBytes()
    }
  }

  def encodeBlocks(): AesBlocksBytes128bitsInterface

  def decodeBlocks(): AesBlocksBytes128bitsInterface

}
