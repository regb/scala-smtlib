package smtlib.sexpr

import Tokens._

/*
 * Note that in theory this should be a complete s-expression parser/lexer, following
 * standard of common lisp. In practice, it is not complete but should supports
 * s-expression as used in SMT-lib (which is a subset of legal s-expression).
 * Hopefully, at some point in the future (in fact, when needed) 
 * it will be fully compliant with s-expression.
 *
 * However, S-expression lacking an actual standard, it is very difficult tu make it a standalone package.
 * So we will just use it as the S-Expression as used in SMTLIB
 */

/*
 * Here is my best shot at fixing the upper case mess, an smtLibCompatibility flag that will
 * parse the symbols as such (on upper casing).
 */
class Lexer(reader: java.io.Reader, smtLibCompatibility: Boolean = false) {

  private def isNewLine(c: Char) = c == '\n' || c == '\r'
  private def isBlank(c: Char) = c == '\n' || c == '\r' || c == ' '
  private def isSeparator(c: Char) = isBlank(c) || c == ')' || c == '('

  //TODO: no lookahead unless asked for read
  private var _currentChar: Int = -1
  private var _futureChar: Option[Int] = None//reader.read

  private def nextChar: Char = {
    _futureChar match {
      case Some(i) => {
        if(_futureChar == -1)
          throw new java.io.EOFException
        _currentChar = i
        _futureChar = None
      }
      case None => {
        try {
          _currentChar = reader.read
        } catch {
          case e: java.io.IOException => throw new java.io.EOFException
        }
        if(_currentChar == -1)
          throw new java.io.EOFException
      }
    }
    _currentChar.toChar
  }
  //peek assumes that there should be something to read, encountering eof
  //should return -1, but at least the call should not be blocked
  private def peek: Int = _futureChar match {
    case Some(i) => i
    case None => {
      try {
        val tmp = reader.read
        _futureChar = Some(tmp)
        tmp
      } catch {
        case e: java.io.IOException => -1
      }
    }
  }

  /* 
     Return the next token if there is one, or null if EOF.
     Throw an EOFException if EOF is reached at an unexpected moment (incomplete token).
  */
  def next: Token = if(peek == -1) null else {

    var c: Char = nextChar
    while(isBlank(c)) {
      if(peek == -1)
        return null
      c = nextChar
    }

    c match {
      case ';' => {
        while(!isNewLine(nextChar))
          ()
        next
      }
      case '(' => OParen
      case ')' => CParen
      case ':' => QualifiedSymbol(None, readSymbol(nextChar))
      case '"' => {
        val buffer = new scala.collection.mutable.ArrayBuffer[Char]
        var c = nextChar
        while(c != '"') {
          if(c == '\\' && (peek == '"' || peek == '\\'))
            c = nextChar
          buffer.append(c)
          c = nextChar
        }
        StringLit(new String(buffer.toArray))
      }
      case '#' => {
        val radix = nextChar
        val base: Int = radix match {
          case 'b' => 2
          case 'o' => 8
          case 'x' => 16
          case d if d.isDigit => {
            val r = readInt(d, 10).toInt
            val ending = nextChar
            if(ending != 'r' && ending != 'R')
              sys.error("expected 'r' termination mark for radix")
            r
          }
          case _ => sys.error("unexpected char: " + radix)
        }
        IntLit(readInt(nextChar, base))
      }
      case d if d.isDigit => { //TODO: a symbol can start with a digit !
        val intPart = readInt(d, 10)
        if(peek != '.')
          IntLit(intPart)
        else {
          nextChar
          var fracPart: Double = 0
          var base = 10
          while(peek.toChar.isDigit) {
            fracPart += nextChar.asDigit
            fracPart *= 10
            base *= 10
          }
          DoubleLit(intPart.toDouble + fracPart/base)
        }
      }
      case s if isSymbolChar(s) || s == '|' => {
        val sym = readSymbol(s)
        if(peek == ':') {
          nextChar
          QualifiedSymbol(Some(sym), readSymbol(nextChar))
        } else SymbolLit(sym)
      }
    }
  }

  private def readSymbol(currentChar: Char): String = {
    val buffer = new scala.collection.mutable.ArrayBuffer[Char]
    if(currentChar == '|') {
      var c = nextChar
      while(c != '|') {
        if(c == '\\')
          c = nextChar
        buffer.append(c)
        c = nextChar
      }
    } else {
      buffer.append(if(smtLibCompatibility) currentChar else currentChar.toUpper)
      while(isSymbolChar(peek.toChar) || peek == '\\') {
        if(peek == '\\') {
          nextChar
          buffer.append(nextChar) //escaped char is not stored in upper case
        } else
        if(smtLibCompatibility)
          buffer.append(nextChar.toUpper)
        else
          buffer.append(nextChar.toUpper)
      }
    }
    new String(buffer.toArray)
  }

  private def readInt(currentChar: Char, r: Int): BigInt = {
    require(r > 1 && r <= 36)
    var acc: BigInt = currentChar.asDigit //asDigit works for 'A', 'F', ...
    while(isDigit(peek.toChar, r)) {
      acc *= r
      acc += nextChar.asDigit
    }
    acc
  }

  private var extraSymbolChars = Set('+', '-', '*', '/', '@', '$', '%', '^', '&', '_', 
                                     '!', '?', '[', ']', '{', '}', '=', '<', '>', '~', '.')
  private def isSymbolChar(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || extraSymbolChars.contains(c)


}
