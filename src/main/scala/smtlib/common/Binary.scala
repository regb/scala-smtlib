package com.on

case class Binary(repr: List[Boolean]) {

  //take the 32 least significant bits
  def toBits: Int = {
    val allReversedBits: List[Boolean] = repr.take(32).reverse.padTo(32, false)
    allReversedBits.foldLeft(0)((bits, bit) => (bits | (if(bit) 1 else 0)) << 2)
  }

  //transform to a 32 bits integer, respecting 2 complements
  def toInt: Int = ???
}
