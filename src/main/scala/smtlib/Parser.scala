package smtlib

import sexpr.SExprs._
import Commands._

import scala.collection.Iterator

object Parser {
  class UnknownCommand(msg: String) extends Exception(msg)

  //def fromString(str: String): S
}

/*
 * TODO: specify what is the behaviour with CAPS from Symbol S-Expr parser
 */

class Parser(input: java.io.Reader) extends Iterator[Command] {

  import Parser._

  private val l = new sexpr.Lexer(input)
  private val p = new sexpr.Parser(l)

  private var lookAhead: Option[SExpr] = None

  override def hasNext: Boolean = {
    lookAhead match {
      case Some(expr) => expr != null
      case None => {
        val c = p.parse
        lookAhead = Some(c)
        c != null
      }
    }
  }

  override def next: Command = {
    val cmd = lookAhead match {
      case None => p.parse
      case Some(c) => {
        lookAhead = None
        c
      }
    }
    if(cmd == null)
      throw new NoSuchElementException
    val res = cmd match {
      case SList(List(SSymbol("set-logic"), SSymbol(logic))) => 
        SetLogic(Logic.fromString(logic))
      case SList(SSymbol("set-info") :: attr) =>
        SetInfo(parseAttribute(attr))
      case SList(SSymbol("set-option") :: option) =>
        SetOption(parseOption(option))
      case SList(List(SSymbol("declare-sort"), s@SSymbol(sort), SInt(arity))) => 
        DeclareSort(sort, arity.toInt)
      case SList(List(SSymbol("declare-fun"), s@SSymbol(fun), SList(sorts), sort)) =>
        DeclareFun(fun, sorts, sort)
      case SList(List(SSymbol("push"), SInt(n))) => 
        Push(n.toInt)
      case SList(List(SSymbol("pop"), SInt(n))) => 
        Pop(n.toInt)
      case SList(List(SSymbol("assert"), term)) =>
        Assert(term)
      case SList(List(SSymbol("check-sat"))) =>
        CheckSat
      case SList(List(SSymbol("get-option"), SQualifiedSymbol(None, SSymbol(keyword)))) => GetOption(keyword)
      case SList(List(SSymbol("get-info"), flag)) => GetInfo(parseInfoFlag(flag))
      case SList(List(SSymbol("exit"))) =>
        Exit
      case _ =>
        throw new UnknownCommand("Unknown command: " + cmd)
    }
    res
  }

  private def parseInfoFlag(flag: SExpr): InfoFlag = flag match {
    case SQualifiedSymbol(None, SSymbol("error-behaviour")) => ErrorBehaviourInfoFlag
    case SQualifiedSymbol(None, SSymbol("name")) => NameInfoFlag
    case SQualifiedSymbol(None, SSymbol("authors")) => AuthorsInfoFlag
    case SQualifiedSymbol(None, SSymbol("version")) => VersionInfoFlag
    case SQualifiedSymbol(None, SSymbol("status")) => StatusInfoFlag
    case SQualifiedSymbol(None, SSymbol("reason-unknown")) => ReasonUnknownInfoFlag
    case SQualifiedSymbol(None, SSymbol("all-statistics")) => AllStatisticsInfoFlag
    case SQualifiedSymbol(None, SSymbol(keyword)) => KeywordInfoFlag(keyword)
    case _ => sys.error("unexpected: " + flag + " when expecting info flag")
  }

  private def parseOption(option: List[SExpr]): SMTOption = option match {
    case List(SQualifiedSymbol(None, SSymbol("print-success")), SBool(bv)) => PrintSuccess(bv)
    case List(SQualifiedSymbol(None, SSymbol("expand-definitions")), SBool(bv)) => ExpandDefinitions(bv)
    case List(SQualifiedSymbol(None, SSymbol("interactive-mode")), SBool(bv)) => InteractiveMode(bv)
    case List(SQualifiedSymbol(None, SSymbol("produce-proofs")), SBool(bv)) => ProduceProofs(bv)
    case List(SQualifiedSymbol(None, SSymbol("produce-unsat-cores")), SBool(bv)) => ProduceUnsatCores(bv)
    case List(SQualifiedSymbol(None, SSymbol("produce-models")), SBool(bv)) => ProduceModels(bv)
    case List(SQualifiedSymbol(None, SSymbol("produce-assignments")), SBool(bv)) => ProduceAssignments(bv)
    case List(SQualifiedSymbol(None, SSymbol("regular-output-channel")), SString(channel)) => RegularOutputChannel(channel)
    case List(SQualifiedSymbol(None, SSymbol("diagnostic-output-channel")), SString(channel)) => DiagnosticOutputChannel(channel)
    case List(SQualifiedSymbol(None, SSymbol("random-seed")), SInt(num)) => RandomSeed(num.toInt)
    case List(SQualifiedSymbol(None, SSymbol("verbosity")), SInt(i)) => Verbosity(i.toInt)
    case _ => AttributeOption(parseAttribute(option))
    //case _ => sys.error("unexpected: " + option + " when expecting option")
  }

  //todo: make sure no nested keyword in value
  private def parseAttribute(ss: List[SExpr]): Attribute = ss match {
    case List(SQualifiedSymbol(None, SSymbol(key))) => Attribute(key, None)
    case List(SQualifiedSymbol(None, SSymbol(key)), v) => Attribute(key, Some(v))
    case _ => sys.error("unexpected: " + ss + " when expecting attribute")
  }

}
