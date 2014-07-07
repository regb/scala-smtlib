package smtlib
package sexpr

import Tokens._
import common._

/*
 * Note that in theory this should be a complete s-expression parser/lexer, following
 * standard of common lisp. In practice, it is not complete but should supports
 * s-expression as used in SMT-lib (which is a subset of legal s-expression).
 *
 * However, S-expression lacking an actual standard, it is very difficult to make 
 * it a standalone package. So we will just make it work for SMT-LIB.
 *
 * The Lexer provides an interface with a next and hasNext.
 * The next function returns the * next available token or throw a runtime exception if
 * the EOF is reached (should call hasNext before invoking next).
 * It throws some self explicit exceptions to indicate an issue with the syntax of the input.
 *
 * The tokens are positioned: the line/column numerotation starts at 1-1.
 */
class Lexer(reader: java.io.Reader) extends Iterator[Token] {

  import Lexer._

  private def isNewLine(c: Char) = c == '\n' || c == '\r'
  private def isBlank(c: Char) = c == '\n' || c == '\r' || c == ' '
  private def isSeparator(c: Char) = isBlank(c) || c == ')' || c == '('

  /*
   * Note that we do not start reading the input until the next function is called.
   */
  private var _currentChar: Int = -1
  private var _futureChar: Option[Int] = None

  /*
   * Current line and column numbers of the current char value
   */
  private var _currentLine: Int = 1
  private var _currentCol: Int = 0

  /*
   * nextChar reads the next char in the reader and convert it into a char.
   * It raises an unexceptedEOFException if EOF is reached in the reader
   */
  private def nextChar: Char = {
    _futureChar match {
      case Some(i) => {
        if(_futureChar == -1)
          throw new UnexpectedEOFException(Position(_currentLine, _currentCol))
        _currentChar = i
        _futureChar = None
      }
      case None => {
        try {
          _currentChar = reader.read
        } catch {
          case e: java.io.EOFException => 
            throw new UnexpectedEOFException(Position(_currentLine, _currentCol))
        }
        if(_currentChar == -1)
          throw new UnexpectedEOFException(Position(_currentLine, _currentCol))
      }
    }

    val res = _currentChar.toChar
    if(isNewLine(res)) {
      _currentLine += 1
      _currentCol = 0
    } else {
      _currentCol += 1
    }
    res
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
        case e: java.io.EOFException => -1
      }
    }
  }

  override def hasNext: Boolean = peek != -1

  /* 
   * Return the next token if there is one, or throw NoSuchElementException.
   */
  override def next: Token = if(peek == -1) {
    throw new NoSuchElementException
  } else {

    var c: Char = nextChar
    while(isBlank(c)) {
      if(peek == -1)
        return null
      c = nextChar
    }

    val currentPosition = Position(_currentLine, _currentCol)

    val res = c match {
      case ';' => {
        while(!isNewLine(nextChar))
          ()
        next
      }
      case '(' => OParen()
      case ')' => CParen()
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
            if(ending != 'r' && ending != 'R') {
              throw new UnexpectedCharException(ending, 
                Position(_currentLine, _currentCol), 
                "expected 'r' termination mark for radix")
            }
            r
          }
          case _ => {
            throw new UnexpectedCharException(radix,
              Position(_currentLine, _currentCol), 
              "'#' should be followed by a radix")
          }
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

    res.setPos(currentPosition)
  }

  /*
   * There is a confusion with the 'official' S-Expression standard that
   * suggest that symbols should be converted to upper case. We do not ignore
   * case in order to be compatible with smtlib common implementations.
   */
  private def readSymbol(currentChar: Char): String = {
    val buffer = new scala.collection.mutable.ArrayBuffer[Char]
    if(currentChar == '|') { //a symbol can be within quotes: |symb|
      var c = nextChar
      while(c != '|') {
        if(c == '\\')
          c = nextChar
        buffer.append(c)
        c = nextChar
      }
    } else {
      buffer.append(currentChar)
      while(isSymbolChar(peek.toChar) || peek == '\\') {
        if(peek == '\\') { 
          /*
	         * Escaped char was intended to be interpreted in its actual case.
	         * Probably not making a lot of sense in the SMT-LIB standard, but we
	         * are ignoring the backslash and recording the escaped char.
	         */
          nextChar
	      }
        buffer.append(nextChar)
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

object Lexer {

  class UnexpectedCharException(char: Char, position: Position, msg: String) extends
    Exception("Encountered unexpected character: '" + char + "' at " + position + ": " + msg)

  class UnexpectedEOFException(position: Position) extends Exception

}
