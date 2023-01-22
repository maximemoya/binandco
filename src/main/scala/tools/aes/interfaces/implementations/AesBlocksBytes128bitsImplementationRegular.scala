package fr.maxime.binandco
package tools.aes.interfaces.implementations

import tools.aes.interfaces.*
import tools.aes.utils.keyExpansionAES128

object AesBlocksBytes128bitsImplementationRegular {

  def of(plainText: String, key128bits: Bytes128): AesBlocksBytes128bitsInterface = {
    val bytes = plainText.getBytes
    aesBlocksBytes128bitsImplementationRegular(getBytesN2(bytes), key128bits)
  }

  def of(bytes: Array[Byte], key128bits: Bytes128): AesBlocksBytes128bitsInterface = {
    aesBlocksBytes128bitsImplementationRegular(getBytesN2(bytes), key128bits)
  }

  private def getBytesN2(bytes: Array[Byte]) = bytes.sliding(16, 16).toArray.map(bytes => Bytes128.of(bytes))

  private def aesBlocksBytes128bitsImplementationRegular(bytesN2: Array[Bytes128], key128bits: Bytes128): AesBlocksBytes128bitsInterface =
    new AesBlocksBytes128bitsInterface {
      override val blocks: Array[AesBytes128bitsInterface] = bytesN2.map(bytes128bits => AesBytes128bitsImplementationRegular.of(bytes128bits.getBytes))
      override val tableEncode: Table16x16 = Table16x16.getAesSubstitutionBOX
      override val tableDecode: Table16x16 = Table16x16.createDecodeTable16x16(tableEncode)
      override val keyExpansionEncode: KeyExpansion128bits = KeyExpansion128bits(key128bits, tableEncode)
      override val galoisFieldEncodeBox: Bytes128 = Bytes128.galoisFieldEncodeBox
      override val galoisFieldDecodeBox: Bytes128 = Bytes128.galoisFieldDecodeBox

      //TODO: code it
      override def encodeBlocks(): AesBlocksBytes128bitsInterface = this

      override def decodeBlocks(): AesBlocksBytes128bitsInterface = this
    }

}

object TryBlock extends App {
  private val myAesBlockOfBytes128 = AesBlocksBytes128bitsImplementationRegular.of("abcdefghij", Bytes128.of("Thats my Kung Fu"))
  myAesBlockOfBytes128.tableEncode.printTable()
  myAesBlockOfBytes128.tableDecode.printTable()
  myAesBlockOfBytes128.galoisFieldEncodeBox.printBytes128()
  myAesBlockOfBytes128.galoisFieldDecodeBox.printBytes128()
  myAesBlockOfBytes128.keyExpansionEncode.printWords()
  myAesBlockOfBytes128.printBlocks()

}