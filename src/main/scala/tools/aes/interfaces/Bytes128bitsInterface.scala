package fr.maxime.binandco
package tools.aes.interfaces

private class Bytes128(bytes:Array[Byte]) {
  def apply(): Array[Byte] = bytes
}
object Bytes128{
  def of(s:String): Bytes128 = {
    val arr = s.getBytes
    if (arr.length > 16){
      new Bytes128(arr.take(15))
    }
    else {
      new Bytes128(arr)
    }
  }
}

private trait Bytes128bitsInterface {
  val bytes128: Bytes128
}
object Bytes128bitsInterface {
  def of(s:String): Bytes128bitsInterface =
    if(s != null){
      new Bytes128bitsInterface {
        override val bytes128: Bytes128 = Bytes128.of(s)
      }
    }
    else{
      throw Error("try to create Bytes128bitsInterface.of(null) <= string can not be null here")
    }

}

val b1:Bytes128bitsInterface = Bytes128bitsInterface.of(null)
object TestIt extends App {

  println(new Bytes128(Array(0,1,2,3,4))().mkString(" "))
  println(b1.bytes128().mkString(" "))

}