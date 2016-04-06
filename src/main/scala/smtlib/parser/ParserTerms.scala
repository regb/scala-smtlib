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
    Attribute(keyword, attributeValue)
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

      if(getPeekToken.kind == Tokens.Underscore) {
        val id = parseUnderscoreIdentifier
        Sort(id)
      } else {

        val name = parseIdentifier

        val subSorts = new ListBuffer[Sort]
        while(getPeekToken.kind != Tokens.CParen)
          subSorts.append(parseSort)
        eat(Tokens.CParen)

        Sort(name, subSorts.toList)
      }
    } else {
      val id = parseIdentifier
      Sort(id)
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
    val indices = parseUntil(Tokens.CParen)(parseSExpr _)

    Identifier(sym, head +: indices)
  }

  def parseQualifiedIdentifier: QualifiedIdentifier = {
    getPeekToken.kind match {
      case Tokens.OParen => {
        eat(Tokens.OParen)
        getPeekToken.kind match {
          case Tokens.As => {
            parseAsIdentifier
          }
          case Tokens.Underscore => {
            QualifiedIdentifier(parseUnderscoreIdentifier)
          }
          case _ => expected(peekToken, Tokens.As, Tokens.Underscore)
        }
      }
      case _ => QualifiedIdentifier(parseIdentifier)
    }
  }

  def parseAsIdentifier: QualifiedIdentifier = {
    eat(Tokens.As)
    val id = parseIdentifier
    val sort = parseSort
    eat(Tokens.CParen)
    QualifiedIdentifier(id, Some(sort))
  }

  def parseIdentifier: Identifier = {
    if(getPeekToken.kind == Tokens.OParen) {
      eat(Tokens.OParen)
      parseUnderscoreIdentifier
    } else {
      val sym = parseSymbol
      Identifier(sym)
    }
  }

  def parseTerm: Term = {
    if(getPeekToken.kind == Tokens.OParen) {
      eat(Tokens.OParen)

      getPeekToken.kind match {
        case Tokens.Let =>
          eat(Tokens.Let)
          val (head, bindings) = parseOneOrMore(parseVarBinding _)
          val term = parseTerm
          eat(Tokens.CParen)
          Let(head, bindings, term)
        case Tokens.Forall =>
          eat(Tokens.Forall)
          val (head, vars) = parseOneOrMore(parseSortedVar _)
          val term = parseTerm
          eat(Tokens.CParen)
          Forall(head, vars, term)
        case Tokens.Exists =>
          eat(Tokens.Exists)
          val (head, vars) = parseOneOrMore(parseSortedVar _)
          val term = parseTerm
          eat(Tokens.CParen)
          Exists(head, vars, term)

        case Tokens.ExclamationMark =>
          eat(Tokens.ExclamationMark)
          val term = parseTerm
          val head = parseAttribute
          val attrs = new ListBuffer[Attribute]
          while(getPeekToken.kind != Tokens.CParen)
            attrs.append(parseAttribute)
          eat(Tokens.CParen)
          AnnotatedTerm(term, head, attrs)

        case Tokens.As =>
          parseAsIdentifier
        case Tokens.Underscore =>
          QualifiedIdentifier(parseUnderscoreIdentifier)

        case _ => //should be function application
          val id = parseQualifiedIdentifier 

          val head = parseTerm

          val terms = new ListBuffer[Term]
          while(getPeekToken.kind != Tokens.CParen)
            terms.append(parseTerm)
          eat(Tokens.CParen)

          FunctionApplication(id, head::terms.toList)
      }
    } else {
      val cst = tryParseConstant
      cst.getOrElse(QualifiedIdentifier(parseIdentifier))
    }
  }

  def parseVarBinding: VarBinding = {
    eat(Tokens.OParen)
    val sym = parseSymbol
    val term = parseTerm
    eat(Tokens.CParen)
    VarBinding(sym, term)
  }
  def parseSortedVar: SortedVar = {
    eat(Tokens.OParen)
    val sym = parseSymbol
    val sort = parseSort
    eat(Tokens.CParen)
    SortedVar(sym, sort)
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

  def parseSList: SList = {
    eat(Tokens.OParen)
    parseSListContent
  }

  //parse s-list assuming the parentheses has been parsed
  protected def parseSListContent: SList = {
    val exprs = new ListBuffer[SExpr]
    while(getPeekToken.kind != Tokens.CParen)
      exprs.append(parseSExpr)
    eat(Tokens.CParen)
    SList(exprs.toList)
  }

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
