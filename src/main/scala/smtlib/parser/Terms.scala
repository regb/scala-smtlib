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


object Terms {

  sealed trait Index
  case class Identifier(symbol: SSymbol, indices: Seq[Index] = Seq()) {

    def isIndexed: Boolean = indices.nonEmpty

  }

  object SimpleIdentifier {
    def apply(symbol: SSymbol) = Identifier(symbol, Seq())
    def unapply(id: Identifier): Option[SSymbol] = id match {
      case Identifier(sym, Seq()) => Some(sym)
      case _ => None
    }
  }


  //(_ map or)
  object ExtendedIdentifier {
    
    def apply(symbol: SSymbol, extension: SSymbol) = Identifier(symbol, Seq(extension))
    
    def unapply(id: Identifier): Option[(SSymbol, SSymbol)] = id match {
      case Identifier(sym, Seq(ext@SSymbol(_))) => Some((sym, ext))
      case _ => None
    }
  }

  case class Sort(id: Identifier, subSorts: Seq[Sort]) {
    override def toString: String = printer.RecursivePrinter.toString(this)
  }
  object Sort {
    def apply(id: Identifier): Sort = Sort(id, Seq())
  }

  case class Attribute(keyword: SKeyword, value: Option[AttributeValue])
  object Attribute {
    def apply(key: SKeyword): Attribute = Attribute(key, None)
  }
  sealed trait AttributeValue extends SExpr

  case class SortedVar(name: SSymbol, sort: Sort)
  case class VarBinding(name: SSymbol, term: Term)


  trait SExpr extends Positioned

  case class SList(sexprs: List[SExpr]) extends SExpr with AttributeValue
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs:_*))
  }
  case class SKeyword(name: String) extends SExpr
  case class SSymbol(name: String) extends SExpr with AttributeValue with Index

  /* SComment is never parsed, only used for pretty printing */
  case class SComment(s: String) extends SExpr 

  sealed abstract class Term extends Positioned with SExpr {
    override def toString: String = printer.RecursivePrinter.toString(this)
  }

  case class Let(binding: VarBinding, bindings: Seq[VarBinding], term: Term) extends Term
  case class ForAll(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term
  case class Exists(sortedVar: SortedVar, sortedVars: Seq[SortedVar], term: Term) extends Term

  case class QualifiedIdentifier(id: Identifier, sort: Option[Sort]) extends Term
  object QualifiedIdentifier {
    def apply(id: Identifier): QualifiedIdentifier = QualifiedIdentifier(id, None)
  }

  case class AnnotatedTerm(term: Term, attribute: Attribute, attributes: Seq[Attribute]) extends Term
  case class FunctionApplication(fun: QualifiedIdentifier, terms: Seq[Term]) extends Term {
    //a function application with no argument is a qualified identifier
    require(terms.nonEmpty)
  }


  sealed trait Constant extends Term with AttributeValue

  sealed trait Literal[T] extends Constant {
    val value: T
  }

  case class SNumeral(value: BigInt) extends Literal[BigInt] with Index
  case class SHexadecimal(value: Hexadecimal) extends Literal[Hexadecimal]
  case class SBinary(value: List[Boolean]) extends Literal[List[Boolean]]
  case class SDecimal(value: BigDecimal) extends Literal[BigDecimal]
  case class SString(value: String) extends Literal[String]

}
