package fr.maxime.binandco
package tools.aes.interfaces.implementations

import tools.aes.interfaces.*

object AesBlocksBytes128bitsImplementationRegular {

  def of(plainText: String, key128bits: Bytes128): AesBlocksBytes128bitsInterface = {
    val bytes = plainText.getBytes
    aesBlocksBytes128bitsImplementationRegular(getBytesN2(bytes).map(byte128 => {
      byte128.reverseBytes128()
      byte128
    }), key128bits)
  }

  def of(bytes: Array[Byte], key128bits: Bytes128): AesBlocksBytes128bitsInterface = {
    aesBlocksBytes128bitsImplementationRegular(getBytesN2(bytes), key128bits)
  }

  private def getBytesN2(bytes: Array[Byte]) : Array[Bytes128] = bytes.sliding(16, 16).toArray.map(bytes => Bytes128.of(bytes))

  private def aesBlocksBytes128bitsImplementationRegular(bytesN2: Array[Bytes128], key128bits: Bytes128): AesBlocksBytes128bitsInterface =
    new AesBlocksBytes128bitsInterface {
      override val blocks: Array[AesBytes128bitsInterface] = bytesN2.map(bytes128bits => AesBytes128bitsImplementationRegular.of(bytes128bits.getBytes))
      override val tableEncode: Table16x16 = Table16x16.getAesSubstitutionBOX
      override val tableDecode: Table16x16 = Table16x16.createDecodeTable16x16(tableEncode)
      override val keyExpansionEncode: KeyExpansion128bits = KeyExpansion128bits(key128bits, tableEncode)
      override val galoisFieldEncodeBox: Bytes128 = Bytes128.galoisFieldEncodeBox
      override val galoisFieldDecodeBox: Bytes128 = Bytes128.galoisFieldDecodeBox
      
      override def encodeBlocks(): AesBlocksBytes128bitsInterface = {

        for(i <- this.blocks.indices){
          this.blocks(i).addRoundKey(this.keyExpansionEncode, 0)
          for (j <- 1 until 10){
            this.blocks(i)
              .subBytes(this.tableEncode)
              .shiftRowsEncode()
              .mixColumns(this.galoisFieldEncodeBox)
              .addRoundKey(this.keyExpansionEncode, j)
          }
          this.blocks(i)
            .subBytes(this.tableEncode)
            .shiftRowsEncode()
            .addRoundKey(this.keyExpansionEncode, 10)
        }
        this

      }
      
      //TODO: code it
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
