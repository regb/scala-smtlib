package smtlib

/*
 * Even though it is a bit annoying to have a layer on top of the S-expression syntax;
 * it seems reasonable to have a well typed SMT-LIB tree to interact with the parser/printer.
 *
 * That choice is also influenced by the somewhat unclear treatment of upper/lower case symbols in SMT-LIB.
 */

object Terms {

  //TODO

  sealed trait Term

  trait Literal[T] extends Term {
    val value: T
  }

  case class Numeral(value: BigInt) extends Literal[BigInt]
  case class Decimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SMTLIBString(value: String) extends Literal[String]
}
