package smtlib
package parser

import lexer._
import common._
import Commands._
import CommandsResponses._
import Terms._
import Parser._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

import scala.language.implicitConversions

class CommandsParserTests extends FunSuite with Timeouts {

  override def suiteName = "SMT-LIB commands Parser suite"


  //parse the string for a single command and asserts no more commands
  private def parseUniqueCmd(str: String): Command = {
    val reader = new StringReader(str)
    val lexer = new Lexer(reader)
    val parser = new Parser(lexer)
    val cmd = parser.parseCommand
    assert(lexer.nextToken == null)
    cmd
  }

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym.name)
  private implicit def intToNum(n: Int): SNumeral = SNumeral(n)
  private implicit def IntSeqToIndices(ns: Seq[Int]): Seq[Index] = ns.map(n => SNumeral(n))



  test("Parsing assert commands") {
    assert(parseUniqueCmd("(assert true)") === 
           Assert(QualifiedIdentifier("true")))
    assert(parseUniqueCmd("(assert (p 42))") === 
           Assert(FunctionApplication(QualifiedIdentifier("p"), 
                  Seq(SNumeral(42)))))
  }

  test("Parsing assert without term throws UnexpectedTokenException") {
    intercept[UnexpectedTokenException] {
      parseUniqueCmd("(assert)")
    }
  }

  test("Parsing check-sat command") {
    assert(parseUniqueCmd("(check-sat)") === CheckSat())
  }

  test("Parsing check-sat-assuming commands") {
    assert(parseUniqueCmd("(check-sat-assuming (a b c))") === 
           CheckSatAssuming(Seq(
             PropLiteral("a", true),
             PropLiteral("b", true),
             PropLiteral("c", true))))
    assert(parseUniqueCmd("(check-sat-assuming ((not a) b (not c)))") === 
           CheckSatAssuming(Seq(
             PropLiteral("a", false),
             PropLiteral("b", true),
             PropLiteral("c", false))))
  }

  test("Parsing declare-const commands") {
    assert(parseUniqueCmd("(declare-const a A)") ===
           DeclareConst(SSymbol("a"), Sort("A")))
    assert(parseUniqueCmd("(declare-const comp (A B))") ===
           DeclareConst(SSymbol("comp"), 
                        Sort("A", Seq(Sort("B")))))
  }


  test("Parsing declare-fun commands") {
    assert(parseUniqueCmd("(declare-fun xyz (A B) C)") ===
           DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C")))
    assert(parseUniqueCmd("(declare-fun xyz () C)") ===
           DeclareFun("xyz", Seq(), Sort("C")))
    assert(parseUniqueCmd("(declare-fun xyz (A B) (C D))") ===
           DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C", Seq(Sort("D")))))
  }

  test("Parsing declare-sort commands") {
    assert(parseUniqueCmd("(declare-sort A 0)") === DeclareSort("A", 0))
    assert(parseUniqueCmd("(declare-sort A 3)") === DeclareSort("A", 3))
  }


  test("Parsing define-fun commands") {
    assert(parseUniqueCmd("(define-fun f ((a A)) B a)") ===
           DefineFun(FunDef("f", Seq(SortedVar("a", Sort("A"))), Sort("B"), QualifiedIdentifier("a"))))
    assert(parseUniqueCmd("(define-fun f ((a A)) B (f a))") ===
           DefineFun(FunDef("f", Seq(SortedVar("a", Sort("A"))), Sort("B"),
                     FunctionApplication(QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"))))))
  }

  test("Parsing define-fun-rec commands") {
    assert(parseUniqueCmd("(define-fun-rec f ((a A)) B a)") ===
           DefineFunRec(FunDef("f", Seq(SortedVar("a", Sort("A"))), Sort("B"), QualifiedIdentifier("a"))))
    assert(parseUniqueCmd("(define-fun-rec f ((a A)) B (f a))") ===
           DefineFunRec(FunDef("f", Seq(SortedVar("a", Sort("A"))), Sort("B"),
                        FunctionApplication(QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"))))))
    assert(parseUniqueCmd("(define-fun-rec f ((a A)) A (f a))") ===
           DefineFunRec(FunDef("f", Seq(SortedVar("a", Sort("A"))), Sort("A"), 
                        FunctionApplication(QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"))))))

  }

  test("Parsing define-funs-rec commands") {
    assert(parseUniqueCmd(
"""(define-funs-rec
      ( (f ((a A)) B) (g ((a A)) B) )
      ( (g a) (f a) )
)""") ===
        DefineFunsRec(
          Seq(FunDec("f", Seq(SortedVar("a", Sort("A"))), Sort("B")),
              FunDec("g", Seq(SortedVar("a", Sort("A"))), Sort("B"))),
          Seq(FunctionApplication(QualifiedIdentifier("g"), Seq(QualifiedIdentifier("a"))),
              FunctionApplication(QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"))))
        )
    )
  }

  test("Parsing define-sort commands") {
    assert(parseUniqueCmd("(define-sort A () B)") ===
           DefineSort("A", Seq(), Sort("B")))
    assert(parseUniqueCmd("(define-sort A (B C) (Array B C))") ===
           DefineSort("A", Seq("B", "C"), 
                      Sort(Identifier("Array"), 
                           Seq(Sort("B"), Sort("C")))))
  }

  test("Parsing echo commands") {
    assert(parseUniqueCmd("""(echo "abcd")""") === Echo(SString("abcd")))
    assert(parseUniqueCmd("""(echo "alpha")""") === Echo(SString("alpha")))
  }
  test("Parsing exit command") {
    assert(parseUniqueCmd("(exit)") === Exit())
  }

  test("Parsing get-assertions command") {
    assert(parseUniqueCmd("(get-assertions)") === GetAssertions())
  }
  test("Parsing get-assignment command") {
    assert(parseUniqueCmd("(get-assignment)") === GetAssignment())
  }

  test("Parsing get-info commands") {
    assert(parseUniqueCmd("(get-info :all-statistics)") === GetInfo(AllStatisticsInfoFlag))
    assert(parseUniqueCmd("(get-info :assertion-stack-levels)") === GetInfo(AssertionStackLevelsInfoFlag))
    assert(parseUniqueCmd("(get-info :authors)") === GetInfo(AuthorsInfoFlag))
    assert(parseUniqueCmd("(get-info :error-behavior)") === GetInfo(ErrorBehaviorInfoFlag))
    assert(parseUniqueCmd("(get-info :name)") === GetInfo(NameInfoFlag))
    assert(parseUniqueCmd("(get-info :reason-unknown)") === GetInfo(ReasonUnknownInfoFlag))
    assert(parseUniqueCmd("(get-info :version)") === GetInfo(VersionInfoFlag))
    assert(parseUniqueCmd("(get-info :custom)") === GetInfo(KeywordInfoFlag("custom")))
  }

  test("Parsing get-model command") {
    assert(parseUniqueCmd("(get-model)") === GetModel())
  }

  test("Parsing get-option commands") {
    assert(parseUniqueCmd("(get-option :keyword)") === GetOption("keyword"))
    assert(parseUniqueCmd("(get-option :custom)") === GetOption("custom"))
  }

  test("Parsing get-proof command") {
    assert(parseUniqueCmd("(get-proof)") === GetProof())
  }
  test("Parsing get-unsat-assumptions command") {
    assert(parseUniqueCmd("(get-unsat-assumptions)") === GetUnsatAssumptions())
  }
  test("Parsing get-unsat-core command") {
    assert(parseUniqueCmd("(get-unsat-core)") === GetUnsatCore())
  }

  test("Parsing get-value commands") {
    assert(parseUniqueCmd("(get-value (x y z))") === GetValue(SSymbol("x"), Seq(SSymbol("y"), SSymbol("z"))))
  }

  //TODO
  //test("get-value expects at least one term") {
  //  intercept[UnexpectedTokenException] {
  //    parseUniqueCmd("(get-value ())")
  //  }
  //}

  test("Parsing pop commands") {
    assert(parseUniqueCmd("(pop 1)") === Pop(1))
    assert(parseUniqueCmd("(pop 2)") === Pop(2))
  }
  test("Parsing push commands") {
    assert(parseUniqueCmd("(push 1)") === Push(1))
    assert(parseUniqueCmd("(push 4)") === Push(4))
  }

  test("Parsing reset command") {
    assert(parseUniqueCmd("(reset)") === Reset())
  }
  test("Parsing reset-assertions command") {
    assert(parseUniqueCmd("(reset-assertions)") === ResetAssertions())
  }

  test("Parsing set-info command") {
    assert(parseUniqueCmd("""(set-info :author "Reg")""") === SetInfo(Attribute(SKeyword("author"), Some(SString("Reg")))))
    assert(parseUniqueCmd("""(set-info :number 42)""") === SetInfo(Attribute(SKeyword("number"), Some(SNumeral(42)))))
    assert(parseUniqueCmd("""(set-info :test)""") === SetInfo(Attribute(SKeyword("test"), None)))
  }

  test("Parsing the different set-logic commands") {
    assert(parseUniqueCmd("(set-logic AUFLIA)")  === SetLogic(AUFLIA))
    assert(parseUniqueCmd("(set-logic AUFLIRA)")  === SetLogic(AUFLIRA))
    assert(parseUniqueCmd("(set-logic AUFNIRA)")  === SetLogic(AUFNIRA))
    assert(parseUniqueCmd("(set-logic LRA)")  === SetLogic(LRA))

    assert(parseUniqueCmd("(set-logic QF_ABV)")  === SetLogic(QF_ABV))
    assert(parseUniqueCmd("(set-logic QF_AUFBV)")  === SetLogic(QF_AUFBV))
    assert(parseUniqueCmd("(set-logic QF_AUFLIA)")  === SetLogic(QF_AUFLIA))
    assert(parseUniqueCmd("(set-logic QF_AX)")  === SetLogic(QF_AX))
    assert(parseUniqueCmd("(set-logic QF_BV)")  === SetLogic(QF_BV))
    assert(parseUniqueCmd("(set-logic QF_IDL)")  === SetLogic(QF_IDL))
    assert(parseUniqueCmd("(set-logic QF_LIA)") === SetLogic(QF_LIA))
    assert(parseUniqueCmd("(set-logic QF_LRA)") === SetLogic(QF_LRA))
    assert(parseUniqueCmd("(set-logic QF_NIA)") === SetLogic(QF_NIA))
    assert(parseUniqueCmd("(set-logic QF_NRA)") === SetLogic(QF_NRA))
    assert(parseUniqueCmd("(set-logic QF_RDL)") === SetLogic(QF_RDL))
    assert(parseUniqueCmd("(set-logic QF_UF)")  === SetLogic(QF_UF))
    assert(parseUniqueCmd("(set-logic QF_UFBV)")  === SetLogic(QF_UFBV))
    assert(parseUniqueCmd("(set-logic QF_UFIDL)")  === SetLogic(QF_UFIDL))
    assert(parseUniqueCmd("(set-logic QF_UFLIA)")  === SetLogic(QF_UFLIA))
    assert(parseUniqueCmd("(set-logic QF_UFLRA)")  === SetLogic(QF_UFLRA))
    assert(parseUniqueCmd("(set-logic QF_UFNRA)")  === SetLogic(QF_UFNRA))

    assert(parseUniqueCmd("(set-logic UFLRA)")  === SetLogic(UFLRA))
    assert(parseUniqueCmd("(set-logic UFNIA)")  === SetLogic(UFNIA))
  }

  test("Parsing non standard set-logic commands") {
    assert(parseUniqueCmd("(set-logic ALL)") === 
      SetLogic(NonStandardLogic(SSymbol("ALL"))))
    assert(parseUniqueCmd("(set-logic MY_COOL_LOGIC)") === 
      SetLogic(NonStandardLogic(SSymbol("MY_COOL_LOGIC"))))
  }

  test("Parsing set-option command") {
    assert(parseUniqueCmd("""(set-option :diagnostic-output-channel "toto")""") === 

                          SetOption(DiagnosticOutputChannel("toto")))
    assert(parseUniqueCmd("(set-option :expand-definitions true)") === SetOption(ExpandDefinitions(true)))
    assert(parseUniqueCmd("(set-option :expand-definitions false)") === SetOption(ExpandDefinitions(false)))
    assert(parseUniqueCmd("(set-option :global-declarations true)") === SetOption(GlobalDeclarations(true)))
    assert(parseUniqueCmd("(set-option :global-declarations false)") === SetOption(GlobalDeclarations(false)))

    assert(parseUniqueCmd("(set-option :interactive-mode true)") === SetOption(InteractiveMode(true)))
    assert(parseUniqueCmd("(set-option :interactive-mode false)") === SetOption(InteractiveMode(false)))

    assert(parseUniqueCmd("(set-option :print-success true)") === SetOption(PrintSuccess(true)))
    assert(parseUniqueCmd("(set-option :print-success false)") === SetOption(PrintSuccess(false)))

    assert(parseUniqueCmd("(set-option :produce-assertions true)") === SetOption(ProduceAssertions(true)))
    assert(parseUniqueCmd("(set-option :produce-assertions false)") === SetOption(ProduceAssertions(false)))
    assert(parseUniqueCmd("(set-option :produce-assignments true)") === SetOption(ProduceAssignments(true)))
    assert(parseUniqueCmd("(set-option :produce-assignments false)") === SetOption(ProduceAssignments(false)))
    assert(parseUniqueCmd("(set-option :produce-models true)") === SetOption(ProduceModels(true)))
    assert(parseUniqueCmd("(set-option :produce-models false)") === SetOption(ProduceModels(false)))
    assert(parseUniqueCmd("(set-option :produce-proofs true)") === SetOption(ProduceProofs(true)))
    assert(parseUniqueCmd("(set-option :produce-proofs false)") === SetOption(ProduceProofs(false)))
    assert(parseUniqueCmd("(set-option :produce-unsat-assumptions true)") === SetOption(ProduceUnsatAssumptions(true)))
    assert(parseUniqueCmd("(set-option :produce-unsat-assumptions false)") === SetOption(ProduceUnsatAssumptions(false)))
    assert(parseUniqueCmd("(set-option :produce-unsat-cores true)") === SetOption(ProduceUnsatCores(true)))
    assert(parseUniqueCmd("(set-option :produce-unsat-cores false)") === SetOption(ProduceUnsatCores(false)))

    assert(parseUniqueCmd("(set-option :random-seed 42)") === SetOption(RandomSeed(42)))
    assert(parseUniqueCmd("(set-option :random-seed 12)") === SetOption(RandomSeed(12)))

    assert(parseUniqueCmd("""(set-option :regular-output-channel "test")""") === 
                          SetOption(RegularOutputChannel("test")))

    assert(parseUniqueCmd("(set-option :reproducible-resource-limit 4)") === SetOption(ReproducibleResourceLimit(4)))
    assert(parseUniqueCmd("(set-option :reproducible-resource-limit 1)") === SetOption(ReproducibleResourceLimit(1)))
    assert(parseUniqueCmd("(set-option :verbosity 4)") === SetOption(Verbosity(4)))
    assert(parseUniqueCmd("(set-option :verbosity 1)") === SetOption(Verbosity(1)))

    assert(parseUniqueCmd("(set-option :custom 42)") === SetOption(AttributeOption(
      Attribute(SKeyword("custom"), Some(SNumeral(42))))))
    assert(parseUniqueCmd("(set-option :my-option)") === SetOption(AttributeOption(
      Attribute(SKeyword("my-option"), None))))
    assert(parseUniqueCmd("""(set-option :custom "abcd")""") === SetOption(AttributeOption(
      Attribute(SKeyword("custom"), Some(SString("abcd"))))))
  }

  test("Attribute value cannot be a kewyord. Should throw UnexpectedTokenException") {
    intercept[UnexpectedTokenException] {
      parseUniqueCmd("""(set-option :custom :abcd)""")
    }
  }

  test("Parsing unknown command throws UnknownCommandException") {
    val reader1 = new StringReader("(alpha beta)")
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    intercept[UnknownCommandException] {
      parser1.parseCommand
    }
  }


  test("Parsing declare-datatypes commands") {
    assert(parseUniqueCmd(
      "(declare-datatypes () ( (A (A1 (a1 Int) (a2 A)) (A2)) ))") ===
      DeclareDatatypes(Seq(
        (SSymbol("A"), Seq(Constructor("A1", 
                            Seq((SSymbol("a1"), Sort("Int")), (SSymbol("a2"), Sort("A")))),
                           Constructor("A2", Seq())
                          ))
      ))
    )

  }

}
