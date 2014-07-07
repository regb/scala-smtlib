package smtlib

/*
 * =========== IMPORTANT ================
 * This file is still work in progress, the hierarchy is not yet used in the library,
 * but is expected to replace the s-expr based interface at some point.
 *
 * Even though it is a bit annoying to have a layer on top of the S-expression syntax;
 * it seems reasonable to have a well typed SMT-LIB tree to interact with the parser/printer.
 *
 * That choice is also influenced by the somewhat unclear treatment of upper/lower case symbols in SMT-LIB.
 */

import Commands._

object Terms {

  sealed trait Term

  trait Literal[T] extends Term {
    val value: T
  }

  //TODO: should we have Hexa and Binary as literals ? The standard seems to make them syntax elements
  //      thus different from their semantics value as integer
  case class Numeral(value: BigInt) extends Literal[BigInt]
  case class Decimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SMTLIBString(value: String) extends Literal[String]

  case class Let(binding: VarBinding, bindings: Seq[VarBinding], term: Term) extends Term
  case class ForAll(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class Exists(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class QualifiedIdentifier(id: Identifier, sort: Option[Sort]) extends Term
  case class AnnotatedTerm(term: Term, attribute: Attribute, attributes: Seq[Attribute]) extends Term
  case class FunctionApplication(fun: QualifiedIdentifier, terms: Seq[Term]) extends Term //TODO: should terms be at leat of length 1 ?


  case class SortedVar(symbol: String, sort: Sort)
  case class VarBinding(symbol: String, term: Term)
}
