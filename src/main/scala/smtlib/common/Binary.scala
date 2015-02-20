package smtlib
package common

case class Binary(repr: List[Boolean]) {

  //take the 32 least significant bits
  def toIntBits: Int = {
    ((toLongBits << 32) >>> 32).toInt
  }

  def toLongBits: Long = {
    val allReversedBits: List[Boolean] = repr.take(64).reverse.padTo(64, false)
    allReversedBits.foldRight(0l)((bit, bits) => ((bits<<1) | (if(bit) 1 else 0)))
  }

  //transform to a 32 bits integer, respecting 2 complements
  def toInt: Int = ???
}
