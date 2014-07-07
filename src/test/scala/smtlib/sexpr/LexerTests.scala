package smtlib
package sexpr

import Tokens._
import common._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class LexerTests extends FunSuite with Timeouts {

  test("eof read") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === IntLit(12))
    assert(!lexer1.hasNext)
  }

  test("integer literals") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    assert(lexer1.hasNext)
    assert(lexer1.next === IntLit(12))
    assert(!lexer1.hasNext)

    val reader2 = new StringReader("#xF")
    val lexer2 = new Lexer(reader2)
    assert(lexer2.hasNext)
    assert(lexer2.next === IntLit(15))

    val reader3 = new StringReader("#o55")
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === IntLit(45))

    val reader4 = new StringReader("#x1F")
    val lexer4 = new Lexer(reader4)
    assert(lexer4.next === IntLit(31))

    val reader5 = new StringReader("123 #x11 #o12")
    val lexer5 = new Lexer(reader5)
    assert(lexer5.hasNext)
    assert(lexer5.next === IntLit(123))
    assert(lexer5.hasNext)
    assert(lexer5.next === IntLit(17))
    assert(lexer5.hasNext)
    assert(lexer5.next === IntLit(10))
    assert(!lexer5.hasNext)

    val reader6 = new StringReader("#16r21")
    val lexer6 = new Lexer(reader6)
    assert(lexer6.next === IntLit(33))
  }

  test("string literals") {
    val reader1 = new StringReader(""" "12" """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === StringLit("12"))

    val reader2 = new StringReader(""" "abc\"def" """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.next === StringLit("abc\"def"))

    val reader3 = new StringReader(""" " abc \" def" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === StringLit(" abc \" def"))

    val reader4 = new StringReader(""" "\"abc\"" """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.next === StringLit("\"abc\""))
  }


  test("symbol literals") {
    val reader1 = new StringReader(""" d12 """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === SymbolLit("d12"))

    val reader2 = new StringReader(""" abc\ def """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.next === SymbolLit("abc def"))

    val reader3 = new StringReader("""  ab\c\ d\ef" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === SymbolLit("abc def"))

    val reader4 = new StringReader(""" |abc deF| """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.next === SymbolLit("abc deF"))

    val reader5 = new StringReader(""" 
|abc
deF| 
""")
    val lexer5 = new Lexer(reader5)
    assert(lexer5.next === SymbolLit(
"""abc
deF"""))
  }

  test("qualified symbols") {
    val reader1 = new StringReader(""" :d12 """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === QualifiedSymbol(None, "d12"))

    val reader2 = new StringReader(""" abc:def """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.next === QualifiedSymbol(Some("abc"), "def"))

    val reader3 = new StringReader("""  ab\c:d\ef" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === QualifiedSymbol(Some("abc"), "def"))

    val reader4 = new StringReader(""" |abc : deF| """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.next === SymbolLit("abc : deF"))

    val reader5 = new StringReader(""" |abc|:|deF| """)
    val lexer5 = new Lexer(reader5)
    assert(lexer5.next === QualifiedSymbol(Some("abc"), "deF"))
  }

  test("lexer compose") {
    val reader1 = new StringReader("""
      (test "test")
    """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === OParen)
    assert(lexer1.next === SymbolLit("test"))
    assert(lexer1.next === StringLit("test"))
    assert(lexer1.next === CParen)


    val reader2 = new StringReader("""
      ) (  42  42.173
    """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.next === CParen)
    assert(lexer2.next === OParen)
    assert(lexer2.next === IntLit(42))
    assert(lexer2.next === DoubleLit(42.173))

    val reader3 = new StringReader("""
      ) ;(  42  42.173
      12 "salut" ; """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === CParen)
    assert(lexer3.next === IntLit(12))
    assert(lexer3.next === StringLit("salut"))
  }

  test("interactive lexer") {
    val pis = new SynchronousPipedReader
    /*
     * Since the pipe is empty, the lexer should not even start to read
     * in the reader. It should only start reading when asked for the next token.
     */
    val lexer = failAfter(3 seconds) { new Lexer(pis) }
    /* 
     * this is impossible for the lexer to determine whether the token is terminated 
     * or if the next char takes time to arrive, so we need some syntactic separation
     * hence the space after 12
     */
    pis.write("12 ")
    assert(lexer.next === IntLit(12))
    pis.write("(")
    assert(lexer.next === OParen)
    pis.write(")")
    assert(lexer.next === CParen)
    pis.write("\"abcd\"")
    assert(lexer.next === StringLit("abcd"))
  }

  test("Positions of tokens") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    val token1 = lexer1.next
    assert(token1 === IntLit(12))
    assert(token1.getPos == Position(1, 1))

    val reader2 = new StringReader("  12 ")
    val lexer2 = new Lexer(reader2)
    val token2 = lexer2.next
    assert(token2 === IntLit(12))
    assert(token2.getPos == Position(1, 3))

    val reader3 = new StringReader("""(test "test")""")
    val lexer3 = new Lexer(reader3)
    val token31 = lexer3.next
    val token32 = lexer3.next
    val token33 = lexer3.next
    val token34 = lexer3.next
    assert(token31 === OParen)
    assert(token31.getPos === Position(1,1))
    assert(token32 === SymbolLit("test"))
    assert(token32.getPos === Position(1,2))
    assert(token33 === StringLit("test"))
    assert(token33.getPos === Position(1,7))
    assert(token34 === CParen)
    assert(token34.getPos === Position(1,13))

    val reader4 = new StringReader(
"""test
  12
 )""")
    val lexer4 = new Lexer(reader4)
    val token41 = lexer4.next
    val token42 = lexer4.next
    val token43 = lexer4.next
    assert(token41 === SymbolLit("test"))
    assert(token41.getPos === Position(1,1))
    assert(token42 === IntLit(12))
    assert(token42.getPos === Position(2,3))
    assert(token43 === CParen)
    assert(token43.getPos === Position(3,2))
  }

  //TODO: testing exceptions and error handling

  /* 
   * Those tests are outaded but were supporting a strict
   * application of the rules of S-Expression symbols in common lisp
   * where the symbol would be converted to upper cases
  test("symbol literals") {
    val reader1 = new StringReader(""" d12 """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.next === SymbolLit("D12"))

    val reader2 = new StringReader(""" abc\ def """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.next === SymbolLit("ABC DEF"))

    val reader3 = new StringReader("""  ab\c\ d\ef" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.next === SymbolLit("ABc DeF"))

    val reader4 = new StringReader(""" |abc deF| """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.next === SymbolLit("abc deF"))

    val reader5 = new StringReader(""" 
|abc
deF| 
""")
    val lexer5 = new Lexer(reader5)
    assert(lexer5.next === SymbolLit(
"""abc
deF"""))
  }
  */
}
