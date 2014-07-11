package smtlib
package parser

import common.Positioned


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

  sealed trait Term extends Positioned


  //an identifier is either a symbol or an indexed symbol: (_ symbol <numeral>+)
  case class Identifier(symbol: SSymbol, ns: Seq[Int])

  case class Let(binding: VarBinding, bindings: Seq[VarBinding], term: Term) extends Term
  case class ForAll(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class Exists(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class QualifiedIdentifier(id: Identifier, sort: Option[Sort]) extends Term
  case class AnnotatedTerm(term: Term, attribute: Attribute, attributes: Seq[Attribute]) extends Term
  case class FunctionApplication(fun: QualifiedIdentifier, terms: Seq[Term]) extends Term //TODO: should terms be at leat of length 1 ?


  case class SortedVar(symbol: String, sort: Sort)
  case class VarBinding(symbol: String, term: Term)


  trait Literal[T] extends Term {
    val value: T
  }

  case class Numeral(value: BigInt) extends Literal[BigInt]
  case class Decimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SMTLIBString(value: String) extends Literal[String]


  sealed trait SExpr extends Term

  case class SList(sexprs: List[SExpr]) extends SExpr
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs:_*))
  }

  case class SNumeral(n: BigInt) extends SExpr
  case class SHexaDecimal(content: String) extends SExpr
  case class SBinary(content: List[Boolean]) extends SExpr
  case class SDecimal(d: Double) extends SExpr
  case class SString(s: String) extends SExpr
  case class SBoolean(v: Boolean) extends SExpr
  case class SSymbol(s: String) extends SExpr
  case class SKeyword(s: SSymbol) extends SExpr

  /* SComment is never parsed, only used for pretty printing */
  case class SComment(s: String) extends SExpr 


}
