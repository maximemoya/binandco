package fr.maxime.binandco
package tools.aes.interfaces

trait AesBlocksBytes128bitsInterface {

  val blocks: Array[AesBytes128bitsInterface]

  //TODO: make class of keyExpansion
  val keyExpansion: Array[Array[Int]]
  val tableEncode: Table16x16
  val tableDecode: Table16x16
  val galoisFieldEncodeBox: Bytes128
  val galoisFieldDecodeBox: Bytes128

  def printBlocks(): Unit = {
    for (i<- blocks.indices){
      println(s"index: $i")
      blocks(i).printBytes()
    }
  }

  def encodeBlocks(): AesBlocksBytes128bitsInterface
  def decodeBlocks(): AesBlocksBytes128bitsInterface

}
