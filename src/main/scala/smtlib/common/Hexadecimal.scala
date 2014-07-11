package smtlib
package common

class Hexadecimal private(val rep: String) {
  //should be normalized to upper cases
  require(rep.forall(c =>
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')
  ))

  def toInt: Int = {
    rep.foldLeft(0)((acc, c) => {
      acc*16 + c.asDigit//asDigit works for 'A', 'F', ...
    })
  }

  override def toString: String = "#x" + rep

  override def equals(that: Any): Boolean = (that != null) && (that match {
    case (h: Hexadecimal) => rep == h.rep
    case _ => false
  })

  override def hashCode: Int = rep.hashCode

}

object Hexadecimal {

  def fromString(str: String): Option[Hexadecimal] = {
    var error = false
    val rep = str.map(c => {
      if(isDigit(c)) 
        c.toUpper
      else {
        error = true
        c
      }
    })
    if(error) None else Some(new Hexadecimal(rep))
  }

  def fromInt(n: Int): Option[Hexadecimal] = {
    if(n < 0) None else {
      ???
    }
  }

  def isDigit(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

}
