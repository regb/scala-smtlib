package smtlib
package parser

import lexer._
import common._
import Commands._
import Terms._
import Parser._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

import scala.language.implicitConversions

class ParserTests extends FunSuite with Timeouts {

  override def suiteName = "SMT-LIB Parser suite"

  //parse the string for a single command and asserts no more commands
  private def parseUniqueCmd(str: String): Command = {
    val reader = new StringReader(str)
    val lexer = new Lexer(reader)
    val parser = new Parser(lexer)
    val cmd = parser.parseCommand
    cmd
  }

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym)

  test("Parsing single commands") {

    assert(parseUniqueCmd("(set-logic QF_UF)") === SetLogic(QF_UF))

    assert(parseUniqueCmd("(declare-sort A 0)") === DeclareSort("A", 0))
    assert(parseUniqueCmd("(define-sort A (B C) (Array B C))") ===
                          DefineSort("A", Seq("B", "C"), 
                                            Sort(Identifier("Array"), Seq(Sort("B"), Sort("C")))
                                    ))
    assert(parseUniqueCmd("(declare-fun xyz (A B) C)") ===
           DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C")))

    assert(parseUniqueCmd("(push 1)") === Push(1))
    assert(parseUniqueCmd("(push 4)") === Push(4))
    assert(parseUniqueCmd("(pop 1)") === Pop(1))
    assert(parseUniqueCmd("(pop 2)") === Pop(2))
    assert(parseUniqueCmd("(assert true)") === Assert(QualifiedIdentifier(SSymbol("true"))))
    assert(parseUniqueCmd("(check-sat)") === CheckSat())

    assert(parseUniqueCmd("(get-assertions)") === GetAssertions())
    assert(parseUniqueCmd("(get-proof)") === GetProof())
    assert(parseUniqueCmd("(get-unsat-core)") === GetUnsatCore())
    assert(parseUniqueCmd("(get-value (x y z))") === GetValue(SSymbol("x"), Seq(SSymbol("y"), SSymbol("z"))))
    assert(parseUniqueCmd("(get-assignment)") === GetAssignment())

    assert(parseUniqueCmd("(get-option :keyword)") === GetOption("keyword"))
    assert(parseUniqueCmd("(get-info :authors)") === GetInfo(AuthorsInfoFlag))

    assert(parseUniqueCmd("(exit)") === Exit())
  }

  test("Parsing set-option command") {
    assert(parseUniqueCmd("(set-option :print-success true)") === SetOption(PrintSuccess(true)))
    assert(parseUniqueCmd("(set-option :print-success false)") === SetOption(PrintSuccess(false)))
    assert(parseUniqueCmd("(set-option :expand-definitions true)") === SetOption(ExpandDefinitions(true)))
    assert(parseUniqueCmd("(set-option :expand-definitions false)") === SetOption(ExpandDefinitions(false)))
    assert(parseUniqueCmd("(set-option :interactive-mode true)") === SetOption(InteractiveMode(true)))
    assert(parseUniqueCmd("(set-option :interactive-mode false)") === SetOption(InteractiveMode(false)))
    assert(parseUniqueCmd("""(set-option :regular-output-channel "test")""") === 
                          SetOption(RegularOutputChannel("test")))
    assert(parseUniqueCmd("""(set-option :diagnostic-output-channel "toto")""") === 
                          SetOption(DiagnosticOutputChannel("toto")))
    assert(parseUniqueCmd("(set-option :random-seed 42)") === SetOption(RandomSeed(42)))
    assert(parseUniqueCmd("(set-option :verbosity 4)") === SetOption(Verbosity(4)))

  }

  test("Parsing set-info command") {
    assert(parseUniqueCmd("""(set-info :author "Reg")""") === SetInfo(Attribute(SKeyword("author"), Some(SString("Reg")))))
    assert(parseUniqueCmd("""(set-info :test)""") === SetInfo(Attribute(SKeyword("test"), None)))
  }

  test("Unknown command") {
    val reader1 = new StringReader("(alpha beta)")
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    intercept[UnknownCommandException] {
      parser1.parseCommand
    }
  }

  test("simple benchmark") {
    val reader1 = new StringReader("""
      (set-logic QF_UF)
      (declare-fun f (Int) Int)
      (declare-fun a () Int)
      (assert (= (f a) a))
      (check-sat)
    """)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.parseCommand === SetLogic(QF_UF))
    assert(parser1.parseCommand === DeclareFun("f", Seq(Sort("Int")), Sort("Int")))
    assert(parser1.parseCommand === DeclareFun("a", Seq(), Sort("Int")))
    assert(parser1.parseCommand === 
           Assert(FunctionApplication(
                    QualifiedIdentifier(SSymbol("=")),
                    Seq(
                      FunctionApplication(
                        QualifiedIdentifier(SSymbol("f")),
                        Seq(QualifiedIdentifier(SSymbol("a")))
                      ),
                      QualifiedIdentifier(SSymbol("a"))
                    )
                  ))
           )
    assert(parser1.parseCommand === CheckSat())

  }

  test("interactive parser") {
    val pis = new SynchronousPipedReader
    val lexer = failAfter(3 seconds) { new Lexer(pis) }
    val parser = failAfter(3 seconds) { new Parser(lexer) }

    pis.write("(set-logic QF_LRA)")
    assert(parser.parseCommand === SetLogic(QF_LRA))

    pis.write("(assert (< 1 3))")
    assert(parser.parseCommand === 
           Assert(FunctionApplication(
             QualifiedIdentifier(SSymbol("<")),
             Seq(
               SNumeral(1), 
               SNumeral(3)
             )
           )))

  }

}
