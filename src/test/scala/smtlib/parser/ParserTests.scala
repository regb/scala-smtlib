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

class ParserTests extends FunSuite with Timeouts {

  override def suiteName = "SMT-LIB Parser suite"

  //parse the string for a single command and asserts no more commands
  private def parseUniqueCmd(str: String): Command = {
    val reader = new StringReader(str)
    val lexer = new Lexer(reader)
    val parser = new Parser(lexer)
    val cmd = parser.parseCommand
    assert(lexer.nextToken == null)
    cmd
  }


  def parseUniqueTerm(str: String): Term = {
    val reader = new StringReader(str)
    val lexer = new Lexer(reader)
    val parser = new Parser(lexer)
    val term = parser.parseTerm
    assert(lexer.nextToken == null)
    term
  }

  def parseUniqueSExpr(str: String): SExpr = {
    val reader = new StringReader(str)
    val lexer = new Lexer(reader)
    val parser = new Parser(lexer)
    val sexpr = parser.parseSExpr
    assert(lexer.nextToken == null)
    sexpr
  }

  private implicit def strToSym(str: String): SSymbol = SSymbol(str)
  private implicit def strToId(str: String): Identifier = Identifier(SSymbol(str))
  private implicit def strToKeyword(str: String): SKeyword = SKeyword(str)
  private implicit def symToTerm(sym: SSymbol): QualifiedIdentifier = QualifiedIdentifier(sym.name)


  test("Parsing attributes") {
    def parseAttribute(str: String): Attribute = {
      val reader = new StringReader(str)
      val lexer = new Lexer(reader)
      val parser = new Parser(lexer)
      val attr = parser.parseAttribute
      attr
    }

    assert(parseAttribute(":test") === Attribute(SKeyword("test")))
    assert(parseAttribute(":key") === Attribute(SKeyword("key")))
    assert(parseAttribute(":abcd") === Attribute(SKeyword("abcd")))
    assert(parseAttribute(":test alpha") === Attribute(SKeyword("test"), Some(SSymbol("alpha"))))
    assert(parseAttribute(":test 42") === Attribute(SKeyword("test"), Some(SNumeral(42))))
    assert(parseAttribute(""":test "hello" """) === Attribute(SKeyword("test"), Some(SString("hello"))))
    assert(parseAttribute(""":test 23.12 """) === Attribute(SKeyword("test"), Some(SDecimal(23.12))))
    assert(parseAttribute(""":test (abc def) """) === 
                          Attribute(SKeyword("test"), 
                                    Some(SList(
                                          List(SSymbol("abc"), SSymbol("def"))))
                                   ))
    assert(parseAttribute(""":left-assoc""") === Attribute(SKeyword("left-assoc")))
    assert(parseAttribute(""":status unsat""") === Attribute(SKeyword("status"), Some(SSymbol("unsat"))))
    assert(parseAttribute(""":my_attribute (humpty dumpty)""") ===  
           Attribute(SKeyword("my_attribute"), Some(SList(List(SSymbol("humpty"), SSymbol("dumpty"))))))
    assert(parseAttribute(""":authors "Jack and Jill" """) === Attribute(SKeyword("authors"), Some(SString("Jack and Jill"))))
  }

  test("Parsing Sorts") {
    def parseSort(str: String): Sort = {
      val reader = new StringReader(str)
      val lexer = new Lexer(reader)
      val parser = new Parser(lexer)
      val sort = parser.parseSort
      sort
    }

    assert(parseSort("A") === Sort("A"))
    assert(parseSort("(A B)") === Sort("A", Seq(Sort("B"))))
    assert(parseSort("(Array From To)") === Sort("Array", Seq(Sort("From"), Sort("To"))))
    assert(parseSort("(_ A 42)") === Sort(Identifier("A", Seq(42))))
    assert(parseSort("(List (Array Int Real))") === 
                     Sort("List", Seq(
                                    Sort("Array", Seq(Sort("Int"), Sort("Real")))
                                  )
                         ))
    assert(parseSort("((_ FixedSizeList 4) Real)") === 
                     Sort(Identifier("FixedSizeList", Seq(4)), Seq(Sort("Real"))))
    assert(parseSort("(Set (_ Bitvec 3))") === Sort(Identifier("Set"), Seq(Sort(Identifier("Bitvec", Seq(3))))))
  }

  test("Parsing Identifiers") {
    def parseId(str: String): Identifier = {
      val reader = new StringReader(str)
      val lexer = new Lexer(reader)
      val parser = new Parser(lexer)
      val id = parser.parseIdentifier
      id
    }

    assert(parseId("abc") === Identifier("abc"))
    assert(parseId("test") === Identifier("test"))
    assert(parseId("(_ a 1)") === Identifier("a", Seq(1)))
    assert(parseId("(_ a 42 12)") === Identifier("a", Seq(42, 12)))

    //non standard syntax used by Z3 for extensions
    assert(parseId("(_ a sym)") === ExtendedIdentifier("a", SSymbol("sym")))
    assert(parseId("(_ map f)") === ExtendedIdentifier("map", SSymbol("f")))

  }

  test("Literal/constant terms are parsed correctly") {
    assert(parseUniqueTerm("42") === SNumeral(42))
    assert(parseUniqueTerm("42.12") === SDecimal(42.12))
    assert(parseUniqueTerm("#xF") === SHexadecimal(Hexadecimal.fromString("F").get))
    assert(parseUniqueTerm("#b1") === SBinary(List(true)))
  }

  test("Correctly parsing identifier and qualfieid identifiers terms") {
    assert(parseUniqueTerm("abc") === QualifiedIdentifier("abc"))
    assert(parseUniqueTerm("eee") === QualifiedIdentifier("eee"))

    assert(parseUniqueTerm("(as abc A)") === QualifiedIdentifier("abc", Some(Sort("A"))))
    assert(parseUniqueTerm("(as aaaa AB)") === QualifiedIdentifier("aaaa", Some(Sort("AB"))))
    assert(parseUniqueTerm("(as aaaa (A B C))") === QualifiedIdentifier("aaaa", Some(Sort("A", Seq(Sort("B"), Sort("C"))))))

    assert(parseUniqueTerm("(_ abc 42)") === QualifiedIdentifier(Identifier("abc", Seq(42))))
    assert(parseUniqueTerm("(_ efg 12)") === QualifiedIdentifier(Identifier("efg", Seq(12))))
  }

  test("Test weird syntax combination of as/_ for identifier") {
    assert(parseUniqueTerm("(as (_ abc 42) A)") === QualifiedIdentifier(Identifier("abc", Seq(42)), Some(Sort("A"))))
  }

  test("Parsing function applications") {
    assert(parseUniqueTerm("(f a b)") === 
           FunctionApplication(
            QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b"))))

    assert(parseUniqueTerm("(f (g a b) c)") === 
           FunctionApplication(
            QualifiedIdentifier("f"), Seq(
              FunctionApplication(
                QualifiedIdentifier("g"), 
                Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b"))
              ),
              QualifiedIdentifier("c"))))
  }

  test("Parsing Let bindings terms") {
    assert(parseUniqueTerm("(let ((a x)) 42)") ===
           Let(VarBinding("a", QualifiedIdentifier("x")), Seq(), SNumeral(42)))

    assert(parseUniqueTerm("(let ((a x)) a)") ===
           Let(VarBinding("a", QualifiedIdentifier("x")), Seq(), QualifiedIdentifier("a")))

    assert(parseUniqueTerm("(let ((a x) (b y)) (f a b))") ===
           Let(VarBinding("a", QualifiedIdentifier("x")), 
               Seq(VarBinding("b", QualifiedIdentifier("y"))), 
               FunctionApplication(QualifiedIdentifier("f"),
                Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b")))))
  }

  test("Let bindings with no binding throws unexpected token exception") {
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(let () 42)")
    }
  }

  test("Let bindings with missing closing parenthesis in binding throws unexpected token exception") {
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(let ((a 2) a)")
    }
  }

  test("Let with binding to complex term works as expected") {
    assert(parseUniqueTerm("(let ((a (f x y))) 42)") ===
           Let(
            VarBinding("a", 
              FunctionApplication(QualifiedIdentifier("f"),
                                  Seq(QualifiedIdentifier("x"), 
                                      QualifiedIdentifier("y")))),
            Seq(),
            SNumeral(42)))
  }


  test("Parsing quantified terms") {
    assert(parseUniqueTerm("(forall ((a A)) a)") ===
           ForAll(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a"))
          )
    assert(parseUniqueTerm("(forall ((a A) (b B) (c C)) (f a c))") ===
           ForAll(SortedVar("a", Sort("A")), 
                  Seq(SortedVar("b", Sort("B")), SortedVar("c", Sort("C"))),
                  FunctionApplication(QualifiedIdentifier("f"),
                    Seq(QualifiedIdentifier("a"), QualifiedIdentifier("c")))))

    assert(parseUniqueTerm("(exists ((a A)) a)") ===
           Exists(SortedVar("a", Sort("A")), Seq(), QualifiedIdentifier("a"))
          )
    assert(parseUniqueTerm("(exists ((a A) (b B) (c C)) (f a c))") ===
           Exists(SortedVar("a", Sort("A")), 
                  Seq(SortedVar("b", Sort("B")), SortedVar("c", Sort("C"))),
                  FunctionApplication(QualifiedIdentifier("f"),
                    Seq(QualifiedIdentifier("a"), QualifiedIdentifier("c")))))
  }

  test("quantified terms with no binding throws unexpected token exception") {
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(forall () true)")
    }
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(exists () true)")
    }
  }

  test("Parsing annotated term") {
    assert(parseUniqueTerm("(! a :note abcd)") ===
           AnnotatedTerm(QualifiedIdentifier("a"), Attribute(SKeyword("note"), Some(SSymbol("abcd"))), Seq())
          )
    assert(parseUniqueTerm("(! (f a) :note abcd)") ===
           AnnotatedTerm(
            FunctionApplication(QualifiedIdentifier("f"), Seq(QualifiedIdentifier("a"))), 
            Attribute(SKeyword("note"), Some(SSymbol("abcd"))), Seq())
          )

  }

  test("Annotated terms with zero annotation throws unexpected token exception") {
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(! a)")
    }
  }

  test("Parsing complicated terms") {
    assert(parseUniqueTerm("((_ f 1) a b)") === 
           FunctionApplication(
            QualifiedIdentifier(Identifier("f", Seq(1))),
            Seq(QualifiedIdentifier("a"), QualifiedIdentifier("b"))))

    assert(
      parseUniqueTerm("(let ((x 42)) (f x a))") ===
      Let(VarBinding("x", SNumeral(42)),
          Seq(),
          FunctionApplication(
            QualifiedIdentifier("f"), 
            Seq(QualifiedIdentifier("x"), QualifiedIdentifier("a")))))

    assert(
      parseUniqueTerm("(=> b (= e (as emptyset (Set Int))))") ===
      FunctionApplication(QualifiedIdentifier("=>"), Seq(
        QualifiedIdentifier("b"),
        FunctionApplication(QualifiedIdentifier("="), Seq(
          QualifiedIdentifier("e"),
          QualifiedIdentifier("emptyset", Some(Sort("Set", Seq(Sort("Int")))))
        ))
      ))
    )
  }

  test("Parsing FunctionApplication without closing parentheses should throw UnexpectedEOFException") {
    intercept[UnexpectedEOFException] {
      parseUniqueTerm("(f a b")
    }
    intercept[UnexpectedEOFException] {
      parseUniqueTerm("(f a (g b)") //more subtle
    }
  }

  test("Parsing FunctionApplication without argument should throw UnexpectedTokenException") {
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(f)")
    }
    intercept[UnexpectedTokenException] {
      parseUniqueTerm("(abcd)")
    }
  }

  test("Parsing s-expressions") {
    assert(parseUniqueSExpr("42") === SNumeral(42))
    assert(parseUniqueSExpr("12.38") === SDecimal(12.38))
    assert(parseUniqueSExpr("#xa1f") === SHexadecimal(Hexadecimal.fromString("a1f").get))
    assert(parseUniqueSExpr("#b1010") === SBinary(List(true, false, true, false)))
    assert(parseUniqueSExpr(""" "hey there" """) === SString("hey there"))
    assert(parseUniqueSExpr("abcd") === SSymbol("abcd"))
    assert(parseUniqueSExpr(":abcd") === SKeyword("abcd"))
    assert(parseUniqueSExpr("(abc def 42)") === 
      SList(SSymbol("abc"), SSymbol("def"), SNumeral(42)))
  }

  test("Parsing single commands") {

    assert(parseUniqueCmd("(set-logic QF_UF)") === SetLogic(QF_UF))

    assert(parseUniqueCmd("(declare-sort A 0)") === DeclareSort("A", 0))
    assert(parseUniqueCmd("(define-sort A (B C) (Array B C))") ===
                          DefineSort("A", Seq("B", "C"), 
                                            Sort(Identifier("Array"), Seq(Sort("B"), Sort("C")))
                                    ))
    assert(parseUniqueCmd("(declare-fun xyz (A B) C)") ===
           DeclareFun("xyz", Seq(Sort("A"), Sort("B")), Sort("C")))
    assert(parseUniqueCmd("(define-fun f ((a A)) B a)") ===
           DefineFun("f", Seq(SortedVar("a", Sort("A"))), Sort("B"), QualifiedIdentifier("a")))

    assert(parseUniqueCmd("(push 1)") === Push(1))
    assert(parseUniqueCmd("(push 4)") === Push(4))
    assert(parseUniqueCmd("(pop 1)") === Pop(1))
    assert(parseUniqueCmd("(pop 2)") === Pop(2))
    assert(parseUniqueCmd("(assert true)") === Assert(QualifiedIdentifier("true")))
    assert(parseUniqueCmd("(check-sat)") === CheckSat())

    assert(parseUniqueCmd("(get-assertions)") === GetAssertions())
    assert(parseUniqueCmd("(get-proof)") === GetProof())
    assert(parseUniqueCmd("(get-unsat-core)") === GetUnsatCore())
    assert(parseUniqueCmd("(get-value (x y z))") === GetValue(SSymbol("x"), Seq(SSymbol("y"), SSymbol("z"))))
    assert(parseUniqueCmd("(get-assignment)") === GetAssignment())

    assert(parseUniqueCmd("(get-option :keyword)") === GetOption("keyword"))

    assert(parseUniqueCmd("(exit)") === Exit())

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

  test("Parsing set-option command") {
    assert(parseUniqueCmd("(set-option :print-success true)") === SetOption(PrintSuccess(true)))
    assert(parseUniqueCmd("(set-option :print-success false)") === SetOption(PrintSuccess(false)))
    assert(parseUniqueCmd("(set-option :expand-definitions true)") === SetOption(ExpandDefinitions(true)))
    assert(parseUniqueCmd("(set-option :expand-definitions false)") === SetOption(ExpandDefinitions(false)))
    assert(parseUniqueCmd("(set-option :interactive-mode true)") === SetOption(InteractiveMode(true)))
    assert(parseUniqueCmd("(set-option :interactive-mode false)") === SetOption(InteractiveMode(false)))

    assert(parseUniqueCmd("(set-option :produce-proofs true)") === SetOption(ProduceProofs(true)))
    assert(parseUniqueCmd("(set-option :produce-proofs false)") === SetOption(ProduceProofs(false)))
    assert(parseUniqueCmd("(set-option :produce-unsat-cores true)") === SetOption(ProduceUnsatCores(true)))
    assert(parseUniqueCmd("(set-option :produce-unsat-cores false)") === SetOption(ProduceUnsatCores(false)))
    assert(parseUniqueCmd("(set-option :produce-models true)") === SetOption(ProduceModels(true)))
    assert(parseUniqueCmd("(set-option :produce-models false)") === SetOption(ProduceModels(false)))
    assert(parseUniqueCmd("(set-option :produce-assignments true)") === SetOption(ProduceAssignments(true)))
    assert(parseUniqueCmd("(set-option :produce-assignments false)") === SetOption(ProduceAssignments(false)))

    assert(parseUniqueCmd("""(set-option :regular-output-channel "test")""") === 
                          SetOption(RegularOutputChannel("test")))
    assert(parseUniqueCmd("""(set-option :diagnostic-output-channel "toto")""") === 
                          SetOption(DiagnosticOutputChannel("toto")))

    assert(parseUniqueCmd("(set-option :random-seed 42)") === SetOption(RandomSeed(42)))
    assert(parseUniqueCmd("(set-option :random-seed 12)") === SetOption(RandomSeed(12)))
    assert(parseUniqueCmd("(set-option :verbosity 4)") === SetOption(Verbosity(4)))
    assert(parseUniqueCmd("(set-option :verbosity 1)") === SetOption(Verbosity(1)))

    assert(parseUniqueCmd("(set-option :custom 42)") === SetOption(AttributeOption(
      Attribute(SKeyword("custom"), Some(SNumeral(42))))))
    assert(parseUniqueCmd("(set-option :my-option)") === SetOption(AttributeOption(
      Attribute(SKeyword("my-option"), None))))
    assert(parseUniqueCmd("""(set-option :custom "abcd")""") === SetOption(AttributeOption(
      Attribute(SKeyword("custom"), Some(SString("abcd"))))))
  }

  test("Parsing get-info command") {
    assert(parseUniqueCmd("(get-info :error-behavior)") === GetInfo(ErrorBehaviorInfoFlag))
    assert(parseUniqueCmd("(get-info :name)") === GetInfo(NameInfoFlag))
    assert(parseUniqueCmd("(get-info :authors)") === GetInfo(AuthorsInfoFlag))
    assert(parseUniqueCmd("(get-info :version)") === GetInfo(VersionInfoFlag))
    assert(parseUniqueCmd("(get-info :status)") === GetInfo(StatusInfoFlag))
    assert(parseUniqueCmd("(get-info :reason-unknown)") === GetInfo(ReasonUnknownInfoFlag))
    assert(parseUniqueCmd("(get-info :all-statistics)") === GetInfo(AllStatisticsInfoFlag))
    assert(parseUniqueCmd("(get-info :custom)") === GetInfo(KeywordInfoFlag("custom")))
  }

  //no (set-option :attribute :value)
  test("Attribute value cannot be a kewyord. Should throw UnexpectedTokenException") {
    intercept[UnexpectedTokenException] {
      parseUniqueCmd("""(set-option :custom :abcd)""")
    }
  }

  test("Parsing set-info command") {
    assert(parseUniqueCmd("""(set-info :author "Reg")""") === SetInfo(Attribute(SKeyword("author"), Some(SString("Reg")))))
    assert(parseUniqueCmd("""(set-info :number 42)""") === SetInfo(Attribute(SKeyword("number"), Some(SNumeral(42)))))
    assert(parseUniqueCmd("""(set-info :test)""") === SetInfo(Attribute(SKeyword("test"), None)))
  }

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

  test("Unknown command") {
    val reader1 = new StringReader("(alpha beta)")
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    intercept[UnknownCommandException] {
      parser1.parseCommand
    }
  }

  test("simple benchmark") {
    val benchmark = """
      (set-logic QF_UF)
      (declare-fun f (Int) Int)
      (declare-fun a () Int)
      (assert (= (f a) a))
      (check-sat)
    """
    val cmd1 = SetLogic(QF_UF)
    val cmd2 = DeclareFun("f", Seq(Sort("Int")), Sort("Int"))
    val cmd3 = DeclareFun("a", Seq(), Sort("Int"))
    val cmd4 =
           Assert(FunctionApplication(
                    QualifiedIdentifier("="),
                    Seq(
                      FunctionApplication(
                        QualifiedIdentifier("f"),
                        Seq(QualifiedIdentifier("a"))
                      ),
                      QualifiedIdentifier("a")
                    )
                  ))
    val cmd5 = CheckSat()

    val reader1 = new StringReader(benchmark)
    val lexer1 = new Lexer(reader1)
    val parser1 = new Parser(lexer1)
    assert(parser1.parseCommand === cmd1)
    assert(parser1.parseCommand === cmd2)
    assert(parser1.parseCommand === cmd3)
    assert(parser1.parseCommand === cmd4)
    assert(parser1.parseCommand === cmd5)

    val reader2 = new StringReader(benchmark)
    val lexer2 = new Lexer(reader2)
    val parser2 = new Parser(lexer2)
    assert(parser2.parseScript === Script(List(cmd1, cmd2, cmd3, cmd4, cmd5)))
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
             QualifiedIdentifier("<"),
             Seq(
               SNumeral(1), 
               SNumeral(3)
             )
           )))

  }

}
