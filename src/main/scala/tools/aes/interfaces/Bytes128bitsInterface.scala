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

trait Bytes128bitsInterface {
  val bytes128: Bytes128
}

val b1:Bytes128bitsInterface = new Bytes128bitsInterface {
  override val bytes128: Bytes128 = Bytes128.of("abc")
}
object TestIt extends App {

  println(new Bytes128(Array(0,1,2,3,4))().mkString(" "))
  println(b1.bytes128().mkString(" "))

}