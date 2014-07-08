package smtlib

import common._
import Commands._
import sexpr.SExprs._
import Parser._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class ParserTests extends FunSuite with Timeouts {

  override def suiteName = "SMT-LIB Commands Parser suite"

  //parse the string for a single command and asserts no more commands
  private def parseUniqueCmd(str: String): Command = {
    val reader = new StringReader(str)
    val parser = new Parser(reader)
    assert(parser.hasNext)
    val cmd = parser.next
    assert(!parser.hasNext)
    cmd
  }

  test("Parsing single commands") {

    assert(parseUniqueCmd("(set-logic QF_UF)") === SetLogic(QF_UF))

    assert(parseUniqueCmd("(declare-sort A 0)") === DeclareSort("A", 0))
    assert(parseUniqueCmd("(define-sort A (B C) (Array B C))") ===
                          DefineSort("A", Seq("B", "C"), 
                                            SList(SSymbol("Array"), SSymbol("B"), SSymbol("C"))
                                    ))
    assert(parseUniqueCmd("(declare-fun xyz (A B) C)") ===
           DeclareFun("xyz", Seq(SSymbol("A"), SSymbol("B")), SSymbol("C")))

    assert(parseUniqueCmd("(push 1)") === Push(1))
    assert(parseUniqueCmd("(push 4)") === Push(4))
    assert(parseUniqueCmd("(pop 1)") === Pop(1))
    assert(parseUniqueCmd("(pop 2)") === Pop(2))
    assert(parseUniqueCmd("(assert true)") === Assert(SBoolean(true)))
    assert(parseUniqueCmd("(check-sat)") === CheckSat)

    assert(parseUniqueCmd("(get-assertions)") === GetAssertions)
    assert(parseUniqueCmd("(get-proof)") === GetProof)
    assert(parseUniqueCmd("(get-unsat-core)") === GetUnsatCore)
    assert(parseUniqueCmd("(get-value (x y z))") === GetValue(SSymbol("x"), Seq(SSymbol("y"), SSymbol("z"))))
    assert(parseUniqueCmd("(get-assignment)") === GetAssignment)

    assert(parseUniqueCmd("(get-option :keyword)") === GetOption("keyword"))
    assert(parseUniqueCmd("(get-info :authors)") === GetInfo(AuthorsInfoFlag))

    assert(parseUniqueCmd("(exit)") === Exit)
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
    assert(parseUniqueCmd("""(set-info :author "Reg")""") === SetInfo(Attribute("author", Some(SString("Reg")))))
    assert(parseUniqueCmd("""(set-info :test)""") === SetInfo(Attribute("test", None)))
  }

  test("Unknown command") {
    val reader1 = new StringReader("(alpha beta)")
    val parser1 = new Parser(reader1)
    assert(parser1.hasNext)
    intercept[UnknownCommandException] {
      parser1.next
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
    val parser1 = new Parser(reader1)
    assert(parser1.hasNext)
    assert(parser1.next === SetLogic(QF_UF))
    assert(parser1.next === DeclareFun("f", Seq(SSymbol("Int")), SSymbol("Int")))
    assert(parser1.next === DeclareFun("a", Seq(), SSymbol("Int")))
    assert(parser1.next === Assert(SList(
                              SSymbol("="), SList(SSymbol("f"), SSymbol("a")), SSymbol("a"))))
    assert(parser1.next === CheckSat)
    assert(!parser1.hasNext)

  }

  test("interactive parser") {
    val pis = new SynchronousPipedReader
    val parser = failAfter(3 seconds) { new Parser(pis) }

    pis.write("(set-logic QF_LRA)")
    assert(parser.hasNext)
    assert(parser.next === SetLogic(QF_LRA))

    pis.write("(assert (< 1 3))")
    assert(parser.hasNext)
    assert(parser.next === Assert(SList(SSymbol("<"), SInt(1), SInt(3))))
  }


  //TODO: testing exceptions and error handling

}
