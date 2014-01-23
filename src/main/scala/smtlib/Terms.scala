package smtlib

/*
 * Even though it is a bit annoying to have a layer on top of the S-expression syntax;
 * it seems reasonable to have a well typed SMT-LIB tree to interact with the parser/printer.
 *
 * That choice is also influenced by the somewhat unclear treatment of upper/lower case symbols in SMT-LIB.
 */

import Commands.{Identifier, Sort} //to remove when id and sort will be moved to here

object Terms {

  //TODO

  sealed trait Term

  trait Literal[T] extends Term {
    val value: T
  }

  //TODO: should we have Hexa and Binary as literals ? The standard seems to make them syntax elements
  //      thus different from their semantics value as integer
  case class Numeral(value: BigInt) extends Literal[BigInt]
  case class Decimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SMTLIBString(value: String) extends Literal[String]

  case class Let(binding: VarBinding, term: Term, terms: Seq[Term]) extends Term
  case class ForAll(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class Exists(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class QualifiedIdentifier(id: Identifier, sort: Option[Sort]) extends Term
  //(QualIdentifier Term+)
  //(! Term Attribute+)


  case class SortedVar(symbol: String, sort: Sort)
  case class VarBinding(symbol: String, term: Term)
}
