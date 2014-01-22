package smtlib.sexpr

import SExprs._
import java.io.StringReader

import org.scalatest.FunSuite

class ParserTests extends FunSuite {

  test("parser basic") {
    val reader1 = new StringReader("""
      (test "test")
    """)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.parse === SList(List(SSymbol("TEST"), SString("test"))))


    val reader2 = new StringReader("""
      (  42  42.173 )
    """)
    val lexer2 = new Lexer(reader2)
    val parser2 = new Parser(lexer2)
    assert(parser2.parse === SList(List(SInt(42), SDouble(42.173))))

    val reader3 = new StringReader("""
      (  42  ("test1" 21))
    """)
    val lexer3 = new Lexer(reader3)
    val parser3 = new Parser(lexer3)
    assert(parser3.parse === SList(List(
      SInt(42), 
      SList(List(SString("test1"),SInt(21))))))

    val reader4 = new StringReader("""
      ()
    """)
    val lexer4 = new Lexer(reader4)
    val parser4 = new Parser(lexer4)
    assert(parser4.parse === SList(List()))
  }

  test("packages") {
    val reader1 = new StringReader("""
      (:test "test")
    """)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.parse === SList(List(SQualifiedSymbol(None, SSymbol("TEST")), SString("test"))))


    val reader2 = new StringReader("""
      (foo:bar :toto)
    """)
    val lexer2 = new Lexer(reader2)
    val parser2 = new Parser(lexer2)
    assert(parser2.parse === 
           SList(List(
             SQualifiedSymbol(Some(SSymbol("FOO")), SSymbol("BAR")),
             SQualifiedSymbol(None, SSymbol("TOTO")))))

  }

}
