package smtlib
package parser

import lexer.Tokens
import Terms._
import Parser._

import scala.collection.mutable.ListBuffer

trait ParserTerms { this: ParserUtils =>

  def parseAttribute: Attribute = {
    val keyword = parseKeyword
    val attributeValue = tryParseAttributeValue
    Attribute(keyword, attributeValue).setPos(keyword)
  }

  def parseAttributeValue: AttributeValue = {
    val attributeValuesTokenKinds: Seq[Tokens.TokenKind] = Seq(
      Tokens.NumeralLitKind, Tokens.BinaryLitKind, Tokens.HexadecimalLitKind, 
      Tokens.DecimalLitKind, Tokens.StringLitKind, Tokens.SymbolLitKind, Tokens.OParen)
    getPeekToken.kind match {
      case Tokens.NumeralLitKind => parseNumeral
      case Tokens.BinaryLitKind => parseBinary
      case Tokens.HexadecimalLitKind => parseHexadecimal
      case Tokens.DecimalLitKind => parseDecimal
      case Tokens.StringLitKind => parseString
      case Tokens.SymbolLitKind => parseSymbol
      case Tokens.OParen => parseSList
      case _ => expected(peekToken, attributeValuesTokenKinds:_*)
    }
  }
  def tryParseAttributeValue: Option[AttributeValue] = {
    if(peekToken == null) None else peekToken.kind match {
      case Tokens.NumeralLitKind => Some(parseNumeral)
      case Tokens.BinaryLitKind => Some(parseBinary)
      case Tokens.HexadecimalLitKind => Some(parseHexadecimal)
      case Tokens.DecimalLitKind => Some(parseDecimal)
      case Tokens.StringLitKind => Some(parseString)
      case Tokens.SymbolLitKind => Some(parseSymbol)
      case Tokens.OParen => Some(parseSList)
      case _ => None
    }
  }

  def parseBool: Boolean = {
    nextToken match {
      case Tokens.SymbolLit("true") => true
      case Tokens.SymbolLit("false") => false
      case t => expected(t) //TODO: not sure how to tell we were expecting one of two specific symbols
    }
  }

  def parseString: SString = {
    nextToken match {
      case t@Tokens.StringLit(s) => {
        val str = SString(s)
        str.setPos(t)
      }
      case t => expected(t, Tokens.StringLitKind)
    }
  }
  def parseSymbol: SSymbol = {
    nextToken match {
      case t@Tokens.SymbolLit(s) => {
        val symbol = SSymbol(s)
        symbol.setPos(t)
      }
      case t => expected(t, Tokens.SymbolLitKind)
    }
  }

  def parseNumeral: SNumeral = {
    nextToken match {
      case t@Tokens.NumeralLit(n) => {
        val num = SNumeral(n)
        num.setPos(t)
      }
      case token => expected(token, Tokens.NumeralLitKind)
    }
  }

  def parseDecimal: SDecimal = {
    nextToken match {
      case t@Tokens.DecimalLit(n) => {
        val dec = SDecimal(n)
        dec.setPos(t)
      }
      case token => expected(token, Tokens.DecimalLitKind)
    }
  }

  def parseKeyword: SKeyword = {
    nextToken match {
      case t@Tokens.Keyword(k) => {
        val keyword = SKeyword(k)
        keyword.setPos(t)
      }
      case token => expected(token, Tokens.KeywordKind)
    }
  }

  def parseHexadecimal: SHexadecimal = {
    nextToken match {
      case t@Tokens.HexadecimalLit(h) => {
        val hexa = SHexadecimal(h)
        hexa.setPos(t)
      }
      case token => expected(token, Tokens.HexadecimalLitKind)
    }
  }

  def parseBinary: SBinary = {
    nextToken match {
      case t@Tokens.BinaryLit(b) => {
        val bin = SBinary(b.toList)
        bin.setPos(t)
      }
      case token => expected(token, Tokens.BinaryLitKind)
    }
  }

  def parseSort: Sort = {
    if(getPeekToken.kind == Tokens.OParen) {
      eat(Tokens.OParen)

      val res = if(getPeekToken.kind == Tokens.Underscore) {
        Sort(parseUnderscoreIdentifier)
      } else {
        val name = parseIdentifier

        val subSorts = parseUntil(Tokens.CParen, eatEnd = false)(parseSort _)

        Sort(name, subSorts.toList)
      }
      eat(Tokens.CParen)
      res
    } else {
      val id = parseIdentifier
      Sort(id).setPos(id)
    }
  }

  def parseIndex: Index = {
    getPeekToken.kind match {
      case Tokens.SymbolLitKind => parseSymbol
      case Tokens.NumeralLitKind => parseNumeral
      case _ => expected(peekToken, Tokens.SymbolLitKind, Tokens.NumeralLitKind)
    }
  }

  def parseUnderscoreIdentifier: Identifier = {
    eat(Tokens.Underscore)
    val sym = parseSymbol

    val head = parseSExpr
    val indices = parseUntil(Tokens.CParen, eatEnd = false)(parseSExpr _)

    Identifier(sym, head +: indices)
  }

  def parseQualifiedIdentifier: QualifiedIdentifier = {
    getPeekToken.kind match {
      case Tokens.OParen => {
        eat(Tokens.OParen)
        val res = getPeekToken.kind match {
          case Tokens.As => {
            parseAsIdentifier
          }
          case Tokens.Underscore => {
            QualifiedIdentifier(parseUnderscoreIdentifier)
          }
          case _ => expected(peekToken, Tokens.As, Tokens.Underscore)
        }
        eat(Tokens.CParen)
        res
      }
      case _ => {
        val id = parseIdentifier
        QualifiedIdentifier(id).setPos(id)
      }
    }
  }

  def parseAsIdentifier: QualifiedIdentifier = {
    eat(Tokens.As)
    val id = parseIdentifier
    val sort = parseSort
    QualifiedIdentifier(id, Some(sort))
  }

  def parseIdentifier: Identifier = {
    if(getPeekToken.kind == Tokens.OParen) {
      parseWithin(Tokens.OParen, Tokens.CParen)(parseUnderscoreIdentifier _)
    } else {
      val sym = parseSymbol
      Identifier(sym).setPos(sym)
    }
  }

  protected def parseTermWithoutParens: Term = getPeekToken.kind match {
    case Tokens.Let =>
      eat(Tokens.Let)
      val (head, bindings) = parseOneOrMore(parseVarBinding _)
      val term = parseTerm
      Let(head, bindings, term)

    case Tokens.Forall =>
      eat(Tokens.Forall)
      val (head, vars) = parseOneOrMore(parseSortedVar _)
      val term = parseTerm
      Forall(head, vars, term)

    case Tokens.Exists =>
      eat(Tokens.Exists)
      val (head, vars) = parseOneOrMore(parseSortedVar _)
      val term = parseTerm
      Exists(head, vars, term)

    case Tokens.ExclamationMark =>
      eat(Tokens.ExclamationMark)
      val term = parseTerm
      val head = parseAttribute
      val attrs = parseUntil(Tokens.CParen, eatEnd = false)(parseAttribute _)
      AnnotatedTerm(term, head, attrs)

    case Tokens.As =>
      parseAsIdentifier

    case Tokens.Underscore =>
      QualifiedIdentifier(parseUnderscoreIdentifier)

    case _ => //should be function application
      val id = parseQualifiedIdentifier 
      val head = parseTerm
      val terms = parseUntil(Tokens.CParen, eatEnd = false)(parseTerm _)
      FunctionApplication(id, head::terms.toList)
  }

  def parseTerm: Term = {
    if(getPeekToken.kind == Tokens.OParen) {
      val startPos = getPeekToken.getPos
      val t = parseWithin(Tokens.OParen, Tokens.CParen)(parseTermWithoutParens _)
      t.setPos(startPos)
    } else {
      val cst = tryParseConstant
      cst.getOrElse({
        val id = parseIdentifier
        QualifiedIdentifier(id).setPos(id)
      })
    }
  }

  def parseVarBinding: VarBinding = {
    val start = eat(Tokens.OParen)
    val sym = parseSymbol
    val term = parseTerm
    eat(Tokens.CParen)
    VarBinding(sym, term).setPos(start)
  }
  def parseSortedVar: SortedVar = {
    val start = eat(Tokens.OParen)
    val sym = parseSymbol
    val sort = parseSort
    eat(Tokens.CParen)
    SortedVar(sym, sort).setPos(start)
  }

  def tryParseConstant: Option[Constant] = {
    getPeekToken.kind match {
      case Tokens.NumeralLitKind => Some(parseNumeral)
      case Tokens.HexadecimalLitKind => Some(parseHexadecimal)
      case Tokens.BinaryLitKind => Some(parseBinary)
      case Tokens.DecimalLitKind => Some(parseDecimal)
      case Tokens.StringLitKind => Some(parseString)
      case _ => None
    }
  }

  def parseSList: SList = SList(parseMany(parseSExpr _).toList)

  /**
    *
    * @note This is slighly inconsistent with the fact that Command and Term inherit
    *       from SExpr, in the sense that this will never return a Command or Term
    *       but rather returns the equivalent SList representation. So no
    *       {{{ SetLogic(QF_LIA) }}} but {{{ SList(SSymbol("set-logic"), SSymbol("QF_LIA")) }}}
    */
  def parseSExpr: SExpr = {
    getPeekToken.kind match {
      case Tokens.SymbolLitKind => parseSymbol
      case (word: Tokens.ReservedWord) => {
        nextToken()
        SSymbol(Tokens.reservedToSymbol(word))
      }
      case Tokens.NumeralLitKind => parseNumeral
      case Tokens.BinaryLitKind => parseBinary
      case Tokens.HexadecimalLitKind => parseHexadecimal
      case Tokens.DecimalLitKind => parseDecimal
      case Tokens.StringLitKind => parseString
      case Tokens.KeywordKind => parseKeyword
      case Tokens.OParen => parseSList
      case kind => 
        expected(peekToken, 
                 Tokens.SymbolLitKind, Tokens.NumeralLitKind, Tokens.BinaryLitKind,
                 Tokens.HexadecimalLitKind, Tokens.DecimalLitKind, Tokens.StringLitKind,
                 Tokens.KeywordKind, Tokens.OParen)
    }
  }

}
