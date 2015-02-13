package smtlib
package parser

import Commands._
import CommandsResponses._
import Terms._
import Parser._

import org.scalatest.FunSuite

import scala.language.implicitConversions

class CommandsResponsesParserTests extends FunSuite {

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym.name)


  override def suiteName = "SMT-LIB command response Parser suite"

  test("Parsing generic response") {
    assert(Parser.fromString("success").parseGenResponse === Success)
    assert(Parser.fromString("unsupported").parseGenResponse === Unsupported)
    assert(Parser.fromString("(error \"this is an error\")").parseGenResponse === Error("this is an error"))
  }

  test("Error response without a message should throw an unexpected token exception ") {
    intercept[UnexpectedTokenException] {
      Parser.fromString("(error)").parseGenResponse
    }
  }

  test("Parsing get-info responses") {
    assert(
      Parser.fromString("""(:error-behavior immediate-exit)""").parseGetInfoResponse ===
      GetInfoResponse(ErrorBehaviorInfoResponse(ImmediateExitErrorBehavior), Seq())
    )
    assert(
      Parser.fromString("""(:error-behavior continued-execution)""").parseGetInfoResponse ===
      GetInfoResponse(ErrorBehaviorInfoResponse(ContinuedExecutionErrorBehavior), Seq())
    )
    assert(
      Parser.fromString("""(:authors "Regis Blanc")""").parseGetInfoResponse ===
      GetInfoResponse(AuthorsInfoResponse("Regis Blanc"), Seq())
    )

    assert(
      Parser.fromString("""(:name "CafeSat")""").parseGetInfoResponse ===
      GetInfoResponse(NameInfoResponse("CafeSat"), Seq())
    )

    assert(
      Parser.fromString("""(:version "2.0.1")""").parseGetInfoResponse ===
      GetInfoResponse(VersionInfoResponse("2.0.1"), Seq())
    )

    assert(
      Parser.fromString("""(:reason-unknown timeout)""").parseGetInfoResponse ===
      GetInfoResponse(ReasonUnknownInfoResponse(TimeoutReasonUnknown), Seq())
    )
    assert(
      Parser.fromString("""(:reason-unknown memout)""").parseGetInfoResponse ===
      GetInfoResponse(ReasonUnknownInfoResponse(MemoutReasonUnknown), Seq())
    )
    assert(
      Parser.fromString("""(:reason-unknown incomplete)""").parseGetInfoResponse ===
      GetInfoResponse(ReasonUnknownInfoResponse(IncompleteReasonUnknown), Seq())
    )

    assert(
      Parser.fromString("""(:custom "abcd")""").parseGetInfoResponse ===
      GetInfoResponse(
        AttributeInfoResponse(
          Attribute(SKeyword("custom"), Some(SString("abcd")))),
        Seq())
    )
  }

  test("Parsing check-sat response") {
    assert(Parser.fromString("sat").parseCheckSatResponse === CheckSatResponse(SatStatus))
    assert(Parser.fromString("unsat").parseCheckSatResponse === CheckSatResponse(UnsatStatus))
    assert(Parser.fromString("unknown").parseCheckSatResponse === CheckSatResponse(UnknownStatus))
  }

  test("Parsing get-assertions response") {
    assert(Parser.fromString("(42 a)").parseGetAssertionsResponse === 
           GetAssertionsResponse(Seq(SNumeral(42), QualifiedIdentifier("a"))))
  }

  test("Get-assertions can parse empty list of assertions") {
    assert(Parser.fromString("()").parseGetAssertionsResponse === 
           GetAssertionsResponse(Seq()))
  }

  test("Parsing get-value response") {
    assert(Parser.fromString("((a 1) (b 42))").parseGetValueResponse === GetValueResponse(Seq(
      (QualifiedIdentifier(Identifier("a")), SNumeral(1)), 
      (QualifiedIdentifier(Identifier("b")), SNumeral(42))
    )))
  }

  //the standard requires at least one value, currently we return empty list
  ignore("get-value response must contains at least one valuation pair") {
    intercept[UnexpectedTokenException] {
      Parser.fromString("()").parseGetValueResponse
    }
  }

  test("Parsing get-assignment response") {
    assert(Parser.fromString("((a true))").parseGetAssignmentResponse ===
      GetAssignmentResponse(Seq((SSymbol("a"), true))))
    assert(Parser.fromString("((b false))").parseGetAssignmentResponse ===
      GetAssignmentResponse(Seq((SSymbol("b"), false))))
    assert(Parser.fromString("((c true) (d false))").parseGetAssignmentResponse ===
      GetAssignmentResponse(Seq((SSymbol("c"), true), (SSymbol("d"), false))))
  }

  test("Parsing get-option constant responses") {
    assert(Parser.fromString("abcd").parseGetOptionResponse ===
      GetOptionResponse(SSymbol("abcd")))
    assert(Parser.fromString("42").parseGetOptionResponse ===
      GetOptionResponse(SNumeral(42)))
    assert(Parser.fromString("77.23").parseGetOptionResponse ===
      GetOptionResponse(SDecimal(77.23)))
    assert(Parser.fromString(""" "abcd" """).parseGetOptionResponse ===
      GetOptionResponse(SString("abcd")))
  }

  test("Parsing get-option response list expressions") {
    assert(Parser.fromString("(a 42)").parseGetOptionResponse ===
      GetOptionResponse(SList(SSymbol("a"), SNumeral(42))))
  }

  test("get-option response should not be a keyword") {
    intercept[UnexpectedTokenException] {
      Parser.fromString(":custom").parseGetOptionResponse
    }
  }

  test("Parsing get-model response") {
    assert(Parser.fromString("(model (define-fun z () Int 0))").parseGetModelResponse === 
      GetModelResponse(List(
        DefineFun("z", Seq(), Sort("Int"), SNumeral(0))))
    )

    assert(Parser.fromString(
"""(model 
  (define-fun z () Int 0)
  (declare-fun a () A)
  (forall ((x A)) x)
)""").parseGetModelResponse === 
      GetModelResponse(List(
        DefineFun("z", Seq(), Sort("Int"), SNumeral(0)),
        DeclareFun("a", Seq(), Sort("A")),
        ForAll(SortedVar("x", Sort("A")), Seq(), QualifiedIdentifier("x"))
      ))
    )
  }

}
