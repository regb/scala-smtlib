package smtlib
package common

class Binary private(val digits: List[Boolean]) {

  //take the 32 least significant bits
  def toIntBits: Int = {
    ((toLongBits << 32) >>> 32).toInt
  }

  def toLongBits: Long = {
    val allReversedBits: List[Boolean] = digits.take(64).reverse.padTo(64, false)
    allReversedBits.foldRight(0l)((bit, bits) => ((bits<<1) | (if(bit) 1 else 0)))
  }

  //transform to a 32 bits integer, respecting 2 complements
  def toInt: Int = {
    val allReversedBits: List[Boolean] = digits.take(32).reverse.padTo(32, digits.head)
    allReversedBits.foldRight(0)((bit, bits) => ((bits<<1) | (if(bit) 1 else 0)))
  }

}

object Binary {

  def apply(digits: List[Boolean]) = new Binary(digits)

  def apply(hexa: Hexadecimal) = new Binary(hexa.toBinary)

}
