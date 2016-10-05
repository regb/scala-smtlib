package smtlib
package extensions.tip

import smtlib.lexer.{Tokens => LT}
import smtlib.parser.Terms._
import smtlib.parser.Commands._

object Tokens {
  import LT.ReservedWord

  case object Lambda extends ReservedWord
  case object Match extends ReservedWord
  case object Case extends ReservedWord
  case object Default extends ReservedWord
  case object At extends ReservedWord
  case object AssertNot extends ReservedWord
}

class Lexer(reader: java.io.Reader) extends lexer.Lexer(reader) {
  import LT.Token

  override protected def toReserved(s: String): Option[Token] = s match {
    case "lambda" => Some(Token(Tokens.Lambda))
    case "match" => Some(Token(Tokens.Match))
    case "case" => Some(Token(Tokens.Case))
    case "default" => Some(Token(Tokens.Default))
    case "@" => Some(Token(Tokens.At))
    case "assert-not" => Some(Token(Tokens.AssertNot))
    case _ => super.toReserved(s)
  }
}

class Parser(lexer: Lexer) extends parser.Parser(lexer) {
  import Terms._
  import Commands._

  override protected def parseTermWithoutParens: Term = getPeekToken.kind match {
    case Tokens.Lambda =>
      eat(Tokens.Lambda)
      val args = parseMany(parseSortedVar _)
      val body = parseTerm
      Lambda(args, body)

    case Tokens.At =>
      eat(Tokens.At)
      val (caller, args) = parseOneOrMore(parseTerm _)
      Application(caller, args)

    case Tokens.Match =>
      eat(Tokens.Match)
      val scrut = parseTerm
      val (caseHead, caseRest) = parseOneOrMore { () =>
        eat(Tokens.Case)
        val pattern = getPeekToken.kind match {
          case Tokens.Default =>
            eat(Tokens.Default)
            Default

          case LT.OParen =>
            eat(LT.OParen)
            val (sym, binders) = parseOneOrMore(parseSymbol _)
            CaseClass(sym, binders)

          case _ =>
            val sym = parseSymbol
            CaseObject(sym)
        }
        val rhs = parseTerm
        Case(pattern, rhs)
      }
      Match(scrut, caseHead +: caseRest)

    case _ => super.parseTermWithoutParens
  }

  private def parseParTerm: (Option[Seq[SSymbol]], Term) = getPeekToken.kind match {
    case LT.OParen =>
      eat(LT.OParen)
      getPeekToken.kind match {
        case LT.Par =>
          eat(LT.Par)
          val tps = parseMany(parseSymbol _)
          val res = parseTerm
          eat(LT.CParen)
          (Some(tps), res)

        case _ =>
          val res = parseTermWithoutParens
          eat(LT.CParen)
          (None, res)
      }
    case _ => (None, parseTerm)
  }

  override protected def parseCommandWithoutParens: Command = getPeekToken.kind match {
    case Tokens.AssertNot =>
      eat(Tokens.AssertNot)
      val (optTps, res) = parseParTerm
      optTps match {
        case Some(tps) => AssertPar(tps, theories.Core.Not(res))
        case None => Assert(theories.Core.Not(res))
      }

    case LT.Assert =>
      eat(LT.Assert)
      val (optTps, res) = parseParTerm
      optTps match {
        case Some(tps) => AssertPar(tps, res)
        case None => Assert(res)
      }

    case LT.DeclareConst =>
      eat(LT.DeclareConst)
      def parseDecl: (SSymbol, Sort) = {
        val sym = parseSymbol
        val sort = parseSort
        (sym, sort)
      }
      getPeekToken.kind match {
        case LT.OParen =>
          eat(LT.OParen)
          eat(LT.Par)
          val tps = parseMany(parseSymbol _)
          val (sym, sort) = parseWithin(LT.OParen, LT.CParen)(parseDecl _)
          eat(LT.CParen)
          DeclareConstPar(tps, sym, sort)
        case _ =>
          val (sym, sort) = parseDecl
          DeclareConst(sym, sort)
      }

    case LT.DeclareFun =>
      eat(LT.DeclareFun)
      def parseDecl: (SSymbol, Seq[Sort], Sort) = {
        val sym = parseSymbol
        val sorts = parseMany(parseSort _)
        val resultSort = parseSort
        (sym, sorts, resultSort)
      }
      getPeekToken.kind match {
        case LT.OParen =>
          eat(LT.OParen)
          eat(LT.Par)
          val tps = parseMany(parseSymbol _)
          val (sym, args, resultSort) = parseWithin(LT.OParen, LT.CParen)(parseDecl _)
          eat(LT.CParen)
          DeclareFunPar(tps, sym, args, resultSort)
        case _ =>
          val (sym, args, resultSort) = parseDecl
          DeclareFun(sym, args, resultSort)
      }

    case LT.DefineFun =>
      eat(LT.DefineFun)
      getPeekToken.kind match {
        case LT.OParen =>
          eat(LT.OParen)
          eat(LT.Par)
          val tps = parseMany(parseSymbol _)
          val funDef = parseWithin(LT.OParen, LT.CParen)(parseFunDef _)
          eat(LT.CParen)
          DefineFunPar(tps, funDef)

        case _ =>
          val funDef = parseFunDef
          DefineFun(funDef)
      }

    case LT.DefineFunRec =>
      eat(LT.DefineFunRec)
      getPeekToken.kind match {
        case LT.OParen =>
          eat(LT.OParen)
          eat(LT.Par)
          val tps = parseMany(parseSymbol _)
          val funDef = parseWithin(LT.OParen, LT.CParen)(parseFunDef _)
          eat(LT.CParen)
          DefineFunRecPar(tps, funDef)

        case _ =>
          val funDef = parseFunDef
          DefineFunRec(funDef)
      }

    case LT.DefineFunsRec =>
      eat(LT.DefineFunsRec)
      val (funDec, funDecs) = parseOneOrMore(() => {
        eat(LT.OParen)
        val funDec = getPeekToken.kind match {
          case LT.Par =>
            eat(LT.Par)
            val tps = parseMany(parseSymbol _)
            val funDec = parseWithin(LT.OParen, LT.CParen)(parseFunDec _)
            Left(FunDecPar(tps, funDec.name, funDec.params, funDec.returnSort))
          case _ =>
            Right(parseFunDec)
        }
        eat(LT.CParen)
        funDec
      })
      val (body, bodies) = parseOneOrMore(parseTerm _)
      assert(funDecs.size == bodies.size)

      if ((funDec +: funDecs).exists(_.isLeft)) {
        DefineFunsRecPar(funDec +: funDecs, body +: bodies)
      } else {
        DefineFunsRec((funDec +: funDecs).map(_.right.get), body +: bodies)
      }

    case LT.DeclareDatatypes =>
      eat(LT.DeclareDatatypes)
      val tps = parseMany(parseSymbol _)
      val datatypes = parseMany(parseDatatypes _)
      DeclareDatatypesPar(tps, datatypes)

    case _ => super.parseCommandWithoutParens
  }
}
