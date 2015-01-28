package smtlib
package printer

import lexer.Lexer
import parser.Terms._
import parser.Commands._
import parser.CommandsResponses._
import parser.Parser

import common._

import java.io.StringReader

import org.scalatest.FunSuite

import scala.language.implicitConversions
import scala.annotation.tailrec

class PrinterTests extends FunSuite {

  override def suiteName = "Printer correctness suite"

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym.name)

  def mkTests(implicit printer: Printer): Unit = {
    val printerName = printer.getClass.getName
    test(printerName + ": printing simple Terms") { testSimpleTerms }
    test(printerName + ": Printing tricky Terms") { testTrickyTerms }
    test(printerName + ": Printing Symbols with weird names") { testWeirdSymbolNames }
    test(printerName + ": Printing composed Terms") { testComposedTerm }
    test(printerName + ": Printing Sorts") { testSorts }
    test(printerName + ": Printing single Commands") { testSingleCommands }
    test(printerName + ": Printing declare-datatypes commands") { testDeclareDatatypes }
    test(printerName + ": Printing Commands Responses") { testCommandsResponses }
  }

  mkTests(RecursivePrinter)
  mkTests(TailPrinter)

  private def checkSort(sort: Sort)(implicit printer: Printer): Unit = {

    val directPrint: String = printer.toString(sort)

    val parser = Parser.fromString(directPrint)
    val parsedAgain: Sort = parser.parseSort
    val printAgain: String = printer.toString(parsedAgain)

    assert(directPrint === printAgain)
    assert(sort === parsedAgain)
  }

  private def checkTerm(term: Term)(implicit printer: Printer): Unit = {

    val directPrint: String = printer.toString(term)

    val parser = Parser.fromString(directPrint)
    val parsedAgain: Term = parser.parseTerm
    val printAgain: String = printer.toString(parsedAgain)

    assert(directPrint === printAgain)
    assert(term === parsedAgain)
  }

  private def checkCommand(cmd: Command)(implicit printer: Printer): Unit = {

    val directPrint: String = printer.toString(cmd)

    val parser = Parser.fromString(directPrint)
    val parsedAgain: Command = parser.parseCommand
    val printAgain: String = printer.toString(parsedAgain)

    assert(directPrint === printAgain)
    assert(cmd === parsedAgain)
  }

  private def check[A](res: A, printer: (A) => String, parser: (String) => A): Unit = {

    val directPrint: String = printer(res)

    val parsedAgain: A = parser(directPrint)
    val printAgain: String = printer(parsedAgain)

    assert(directPrint === printAgain)
    assert(res === parsedAgain)
  }


  def testSimpleTerms(implicit printer: Printer): Unit = {
    checkTerm(SNumeral(0))
    checkTerm(SNumeral(42))
    checkTerm(SHexadecimal(Hexadecimal.fromString("FF").get))
    checkTerm(SHexadecimal(Hexadecimal.fromString("123abcDeF").get))
    checkTerm(SBinary(List(true)))
    checkTerm(SBinary(List(true, false, true)))
    checkTerm(SBinary(List(false, false, true)))
    checkTerm(SString("abcd"))
    checkTerm(SString("hello-world"))
    checkTerm(QualifiedIdentifier("abc"))
    checkTerm(FunctionApplication(
            QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b"))))
    checkTerm(Let(VarBinding("a", QualifiedIdentifier("x")), Seq(), QualifiedIdentifier("a")))

    checkTerm(ForAll(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a")))
    checkTerm(Exists(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a")))
    checkTerm(AnnotatedTerm(QualifiedIdentifier("a"), Attribute(SKeyword("note"), Some(SSymbol("abcd"))), Seq()))

  }

  def testTrickyTerms(implicit printer: Printer): Unit = {
    checkTerm(SString("abc\"def"))
    checkTerm(SString("hello \"World\""))
  }

  def testWeirdSymbolNames(implicit printer: Printer): Unit = {
    checkTerm(QualifiedIdentifier("+-/"))
    checkTerm(QualifiedIdentifier("^^^"))
    checkTerm(QualifiedIdentifier("+-/*=%?!.$_~&^<>@"))
    checkTerm(QualifiedIdentifier("$12%"))
  }

  def testComposedTerm(implicit printer: Printer): Unit = {
    checkTerm(
      FunctionApplication(
        QualifiedIdentifier("f"),
        Seq(
          FunctionApplication(
            QualifiedIdentifier("g"),
            Seq(QualifiedIdentifier("aaa"),
                QualifiedIdentifier("bb"))
          ),
          QualifiedIdentifier("c")
        )
      )
    )

    checkTerm(
      FunctionApplication(
        QualifiedIdentifier("f"),
        Seq(
          FunctionApplication(
            QualifiedIdentifier("g"),
            Seq(QualifiedIdentifier("aaa"),
                QualifiedIdentifier("bb"))
          ),
          QualifiedIdentifier("c"),
          FunctionApplication(
            QualifiedIdentifier("g"),
            Seq(QualifiedIdentifier("abcd"),
                FunctionApplication(
                  QualifiedIdentifier("h"),
                  Seq(QualifiedIdentifier("x"))
                ))
          )
        )
      )
    )
  }

  def testSorts(implicit printer: Printer): Unit = {
    checkSort(Sort(Identifier(SSymbol("A"))))
    checkSort(Sort(Identifier(SSymbol("A"), Seq(42))))
    checkSort(Sort(Identifier(SSymbol("A"), Seq(42, 23))))
    checkSort(Sort(Identifier(SSymbol("A"), Seq(42, 12, 23))))
    checkSort(Sort(Identifier(SSymbol("A")), Seq(Sort("B"), Sort("C"))))
    checkSort(Sort(Identifier(SSymbol("A"), Seq(27)), Seq(Sort("B"), Sort("C"))))
  }

  def testSingleCommands(implicit printer: Printer): Unit = {
    checkCommand(SetLogic(QF_UF))

    checkCommand(DeclareSort("A", 0))
    checkCommand(DefineSort("A", Seq("B", "C"), 
                 Sort(Identifier("Array"), Seq(Sort("B"), Sort("C")))))
    checkCommand(DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C")))
    checkCommand(DefineFun("f", Seq(SortedVar("a", Sort("A"))), Sort("B"), QualifiedIdentifier("a")))

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

  def testDeclareDatatypes(implicit printer: Printer): Unit = {
    checkCommand(DeclareDatatypes(Seq(
      SSymbol("A") -> Seq(Constructor(SSymbol("A1"), 
                                      Seq(SSymbol("a1a") -> Sort("A"), SSymbol("a1b") -> Sort("A"))),
                          Constructor(SSymbol("A2"), 
                                      Seq(SSymbol("a2a") -> Sort("A"), SSymbol("a2b") -> Sort("A"))))
    )))
  }


  def testCommandsResponses(implicit printer: Printer): Unit = {

    def printGenRes(res: GenResponse): String = RecursivePrinter.toString(res) 
    def parseGenRes(in: String): GenResponse = Parser.fromString(in).parseGenResponse
    check(Success, printGenRes, parseGenRes)
    check(Unsupported, printGenRes, parseGenRes)
    check(Error("symbol missing"), printGenRes, parseGenRes)

    def printGetAssignRes(res: GetAssignmentResponse): String = RecursivePrinter.toString(res)
    def parseGetAssignRes(in: String): GetAssignmentResponse = Parser.fromString(in).parseGetAssignmentResponse
    //TODO: some tests with get-assignment

    def printCheckSat(res: CheckSatResponse): String = RecursivePrinter.toString(res) 
    def parseCheckSat(in: String): CheckSatResponse = Parser.fromString(in).parseCheckSatResponse
    check(CheckSatResponse(SatStatus), printCheckSat, parseCheckSat)
    check(CheckSatResponse(UnsatStatus), printCheckSat, parseCheckSat)
    check(CheckSatResponse(UnknownStatus), printCheckSat, parseCheckSat)

    def printGetValue(res: GetValueResponse): String = RecursivePrinter.toString(res)
    def parseGetValue(in: String): GetValueResponse = Parser.fromString(in).parseGetValueResponse

    check(GetValueResponse(Seq( 
           (SSymbol("a"), SNumeral(42)) 
          )), printGetValue, parseGetValue)
    check(GetValueResponse(Seq( 
           (SSymbol("a"), SNumeral(42)), 
           (SSymbol("b"), SNumeral(12)) 
         )), printGetValue, parseGetValue)
  }

  test("Printing deep trees with tail printer") {
    @tailrec
    def mkDeepTerm(n: Int, t: Term): Term = 
      if(n == 0) t
      else mkDeepTerm(n-1, Let(VarBinding("x", SString("some value")), Seq(), t))

    val t1 = mkDeepTerm(10000, SString("base case"))

    //will overflow with RecursivePrinter if stack is not too big
    //RecursivePrinter.toString(t1)

    //should not generate exception
    TailPrinter.toString(t1)

    //val t2 = mkDeepTerm(100000, SString("base case"))
    //TailPrinter.toString(t2)
  }


}
