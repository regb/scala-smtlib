package smtlib
package parser

import lexer.Tokens
import Tokens.{Token, TokenKind}
import lexer.Lexer

import Terms._
import Commands._
import CommandsResponses._

import common._

import scala.collection.mutable.ListBuffer

trait ParserCommands { this: ParserUtils with ParserTerms =>

  import Parser._

  def parseScript: Script = {

    var cmds = new ListBuffer[Command]()
    var cmd = parseCommand
    while(cmd != null) {
      cmds.append(cmd)
      cmd = parseCommand
    }
    Script(cmds.toList)
  }

  def parseCommand: Command = if(peekToken == null) null else {
    val head = nextToken
    check(head, Tokens.OParen)

    val cmdNameToken = nextToken
    val cmd = cmdNameToken.kind match {

      case Tokens.Assert => {
        Assert(parseTerm)
      }
      case Tokens.CheckSat => CheckSat()
      case Tokens.CheckSatAssuming => {
        val props = parseMany(parsePropLit _)
        CheckSatAssuming(props)
      }


      case Tokens.DeclareConst => {
        val name = parseSymbol
        val sort = parseSort
        DeclareConst(name, sort)
      }
      case Tokens.DeclareFun => {
        val sym = parseSymbol

        val params = new ListBuffer[Sort]
        eat(Tokens.OParen)
        while(peekToken.kind != Tokens.CParen)
          params.append(parseSort)
        eat(Tokens.CParen)

        val sort = parseSort
        DeclareFun(sym, params.toList, sort)
      }
      case Tokens.DeclareSort => {
        val sym = parseSymbol
        val arity = parseNumeral
        DeclareSort(sym, arity.value.toInt)
      }

      case Tokens.DefineFun => {
        val funDef = parseFunDef
        DefineFun(funDef)
      }
      case Tokens.DefineFunRec => {
        val funDef = parseFunDef
        DefineFunRec(funDef)
      }
      case Tokens.DefineFunsRec => {
        val (funDef, funDefs) = parseOneOrMore(parseFunDec _)
        val (body, bodies) = parseOneOrMore(parseTerm _)
        assert(funDefs.size == bodies.size)
        DefineFunsRec(funDef +: funDefs, body +: bodies)
      }

      case Tokens.SetOption => {
        SetOption(parseOption)
      }
      case Tokens.SetInfo => {
        SetInfo(parseAttribute)
      }
      case Tokens.DefineSort => {
        val sym = parseSymbol

        val vars = new ListBuffer[SSymbol]
        eat(Tokens.OParen)
        while(peekToken.kind != Tokens.CParen)
          vars.append(parseSymbol)
        eat(Tokens.CParen)

        val sort = parseSort
        DefineSort(sym, vars.toList, sort)
      }
      case Tokens.Push => {
        val n = parseNumeral
        Push(n.value.toInt)
      }
      case Tokens.Pop => {
        val n = parseNumeral
        Pop(n.value.toInt)
      }

      case Tokens.GetAssertions => GetAssertions()
      case Tokens.GetProof => GetProof()
      case Tokens.GetUnsatCore => GetUnsatCore()
      case Tokens.GetValue => {
        eat(Tokens.OParen)
        val ts = new ListBuffer[Term]
        while(peekToken.kind != Tokens.CParen)
          ts.append(parseTerm)
        eat(Tokens.CParen)
        GetValue(ts.head, ts.tail.toList)
      }
      case Tokens.GetAssignment => GetAssignment()

      case Tokens.GetOption => {
        val keyword = parseKeyword
        GetOption(keyword)
      }
      case Tokens.GetInfo => {
        val infoFlag = parseInfoFlag
        GetInfo(infoFlag)
      }

      case Tokens.Exit => Exit()

      case Tokens.SetLogic => {
        val logicSymbol: SSymbol = parseSymbol
        val logic: Logic = 
          Logic.standardLogicFromString.lift(logicSymbol.name).getOrElse(NonStandardLogic(logicSymbol))
        SetLogic(logic)
      }

      case Tokens.DeclareDatatypes => {
        eat(Tokens.OParen)
        eat(Tokens.CParen)

        val datatypes = parseMany(parseDatatypes _)

        DeclareDatatypes(datatypes)
      }

      case kind => {
        throw new UnknownCommandException(kind)
      }
    }
    eat(Tokens.CParen)

    cmd.setPos(head)
  }

  def parsePropLit: PropLiteral = {
    peekToken.kind match {
      case Tokens.SymbolLitKind => {
        PropLiteral(parseSymbol, true)
      }
      case Tokens.OParen => {
        eat(Tokens.OParen)
        eat(Tokens.SymbolLit("not"))
        val sym = parseSymbol
        eat(Tokens.CParen)
        PropLiteral(sym, false)
      }
      case _ => {
        expected(peekToken, Tokens.SymbolLitKind, Tokens.OParen)
      }
    }
  }

  def parseFunDec: FunDec = {
    eat(Tokens.OParen)
    val name = parseSymbol

    val sortedVars = parseMany(parseSortedVar _)

    val sort = parseSort
    eat(Tokens.CParen)

    FunDec(name, sortedVars, sort)
  }

  def parseFunDef: FunDef = {
    val name = parseSymbol

    val sortedVars = parseMany(parseSortedVar _)

    val sort = parseSort

    val body = parseTerm

    FunDef(name, sortedVars, sort, body)
  }

  def parseDatatypes: (SSymbol, Seq[Constructor]) = {
    eat(Tokens.OParen)
    val name = parseSymbol
    var constructors = new ListBuffer[Constructor]
    while(peekToken.kind != Tokens.CParen) {
      constructors.append(parseConstructor)
    }
    eat(Tokens.CParen)
    (name, constructors)
  }

  def parseConstructor: Constructor = {
    eat(Tokens.OParen)
    val name = parseSymbol

    var fields = new ListBuffer[(SSymbol, Sort)]
    while(peekToken.kind != Tokens.CParen) {
      eat(Tokens.OParen)
      val fieldName = parseSymbol
      val fieldSort = parseSort
      eat(Tokens.CParen)
      fields.append((fieldName, fieldSort))
    }
    eat(Tokens.CParen)

    Constructor(name, fields.toList)
  }


  def parseInfoFlag: InfoFlag = {
    nextToken match {
      case Tokens.Keyword("error-behavior") => ErrorBehaviorInfoFlag
      case Tokens.Keyword("name") => NameInfoFlag
      case Tokens.Keyword("authors") => AuthorsInfoFlag
      case Tokens.Keyword("version") => VersionInfoFlag
      case Tokens.Keyword("reason-unknown") => ReasonUnknownInfoFlag
      case Tokens.Keyword("all-statistics") => AllStatisticsInfoFlag
      case Tokens.Keyword(keyword) => KeywordInfoFlag(keyword)
      case t => expected(t, Tokens.KeywordKind)
    }
  }


  def parseOption: SMTOption = {
    peekToken match {
      case Tokens.Keyword("print-success") =>
        nextToken
        PrintSuccess(parseBool)
      case Tokens.Keyword("expand-definitions") => 
        nextToken
        ExpandDefinitions(parseBool)
      case Tokens.Keyword("interactive-mode") => 
        nextToken
        InteractiveMode(parseBool)
      case Tokens.Keyword("produce-proofs") => 
        nextToken
        ProduceProofs(parseBool)
      case Tokens.Keyword("produce-unsat-cores") => 
        nextToken
        ProduceUnsatCores(parseBool)
      case Tokens.Keyword("produce-models") => 
        nextToken
        ProduceModels(parseBool)
      case Tokens.Keyword("produce-assignments") => 
        nextToken
        ProduceAssignments(parseBool)
      case Tokens.Keyword("regular-output-channel") => 
        nextToken
        RegularOutputChannel(parseString.value)
      case Tokens.Keyword("diagnostic-output-channel") => 
        nextToken
        DiagnosticOutputChannel(parseString.value)
      case Tokens.Keyword("random-seed") => 
        nextToken
        RandomSeed(parseNumeral.value.toInt)
      case Tokens.Keyword("verbosity") => 
        nextToken
        Verbosity(parseNumeral.value.toInt)
      case _ => 
        AttributeOption(parseAttribute)
    }
  }

}
