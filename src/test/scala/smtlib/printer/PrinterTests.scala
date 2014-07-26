package smtlib
package printer

import lexer.Lexer
import parser.Terms._
import parser.Commands._
import parser.Parser

import common._

import java.io.StringReader

import org.scalatest.FunSuite

import scala.language.implicitConversions

class PrinterTests extends FunSuite {

  override def suiteName = "Printer suite"

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym.name)


  private def checkTerm(term: Term): Unit = {

    val directPrint: String = PrettyPrinter.toString(term)

    val parser = Parser.fromString(directPrint)
    val parsedAgain: Term = parser.parseTerm
    val printAgain: String = PrettyPrinter.toString(parsedAgain)

    assert(directPrint === printAgain)
    assert(term === parsedAgain)
  }

  private def checkCommand(cmd: Command): Unit = {

    val directPrint: String = PrettyPrinter.toString(cmd)

    val parser = Parser.fromString(directPrint)
    val parsedAgain: Command = parser.parseCommand
    val printAgain: String = PrettyPrinter.toString(parsedAgain)

    assert(directPrint === printAgain)
    assert(cmd === parsedAgain)
  }


  test("Printing simple Terms") {

    checkTerm(SNumeral(42))
    checkTerm(QualifiedIdentifier("abc"))
    checkTerm(FunctionApplication(
            QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b"))))
    checkTerm(Let(VarBinding("a", QualifiedIdentifier("x")), Seq(), QualifiedIdentifier("a")))

    checkTerm(ForAll(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a")))
    checkTerm(Exists(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a")))
    checkTerm(AnnotatedTerm(QualifiedIdentifier("a"), Attribute(SKeyword("note"), Some(SSymbol("abcd"))), Seq()))

  }

  test("Printing composed Terms") {

  }

  test("Printing single commands") {

    checkCommand(SetLogic(QF_UF))

    checkCommand(DeclareSort("A", 0))
    checkCommand(DefineSort("A", Seq("B", "C"), 
                 Sort(Identifier("Array"), Seq(Sort("B"), Sort("C")))))
    checkCommand(DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C")))

    checkCommand(Push(1))
    checkCommand(Push(4))
    checkCommand(Pop(1))
    checkCommand(Pop(2))
    checkCommand(Assert(QualifiedIdentifier("true")))
    checkCommand(CheckSat())

    checkCommand(GetAssertions())
    checkCommand(GetProof())
    checkCommand(GetUnsatCore())
    checkCommand(GetValue(SSymbol("x"), Seq(SSymbol("y"), SSymbol("z"))))
    checkCommand(GetAssignment())

    checkCommand(GetOption("keyword"))
    checkCommand(GetInfo(AuthorsInfoFlag))

    checkCommand(Exit())
  }


}
