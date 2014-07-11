package smtlib
package parser

import common._


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

  //an identifier is either a symbol or an indexed symbol: (_ symbol <numeral>+)
  case class Identifier(symbol: SSymbol, ns: Seq[Int]) {
    def isIndexed: Boolean = !ns.isEmpty
  }

  case class Sort(id: Identifier, subSorts: Seq[Sort])

  case class Attribute(keyword: SKeyword, v: Option[SExpr])

  case class SortedVar(symbol: SSymbol, sort: Sort)
  case class VarBinding(symbol: SSymbol, term: Term)


  sealed trait SExpr extends Positioned

  case class SList(sexprs: List[SExpr]) extends SExpr
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs:_*))
  }
  case class SKeyword(name: SSymbol) extends SExpr
  case class SSymbol(name: String) extends SExpr

  /* SComment is never parsed, only used for pretty printing */
  case class SComment(s: String) extends SExpr 

  sealed trait Term extends Positioned with SExpr

  case class Let(binding: VarBinding, bindings: Seq[VarBinding], term: Term) extends Term
  case class ForAll(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class Exists(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class QualifiedIdentifier(id: Identifier, sort: Option[Sort]) extends Term
  case class AnnotatedTerm(term: Term, attribute: Attribute, attributes: Seq[Attribute]) extends Term
  case class FunctionApplication(fun: QualifiedIdentifier, terms: Seq[Term]) extends Term //TODO: should terms be at leat of length 1 ?

  trait Literal[T] extends Term {
    val value: T
  }

  case class SNumeral(value: BigInt) extends Literal[BigInt]
  case class SHexaDecimal(value: Hexadecimal) extends Literal[Hexadecimal]
  case class SBinary(value: List[Boolean]) extends Literal[List[Boolean]]
  case class SDecimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SString(value: String) extends Literal[String]
  case class SBoolean(value: Boolean) extends Literal[Boolean]



}
