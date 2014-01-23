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
      case SList(List(SSymbol("SET-LOGIC"), SSymbol(logic))) => 
        SetLogic(Logic.fromString(logic))
      case SList(SSymbol("SET-INFO") :: attr) =>
        SetInfo(parseAttribute(attr))
      case SList(SSymbol("SET-OPTION") :: option) =>
        SetOption(parseOption(option))
      case SList(List(SSymbol("DECLARE-SORT"), s@SSymbol(sort), SInt(arity))) => 
        DeclareSort(sort, arity.toInt)
      case SList(List(SSymbol("DECLARE-FUN"), s@SSymbol(fun), SList(sorts), sort)) =>
        DeclareFun(fun, sorts, sort)
      case SList(List(SSymbol("PUSH"), SInt(n))) => 
        Push(n.toInt)
      case SList(List(SSymbol("POP"), SInt(n))) => 
        Pop(n.toInt)
      case SList(List(SSymbol("ASSERT"), term)) =>
        Assert(term)
      case SList(List(SSymbol("CHECK-SAT"))) =>
        CheckSat
      case SList(List(SSymbol("GET-OPTION"), SQualifiedSymbol(None, SSymbol(keyword)))) => GetOption(keyword)
      case SList(List(SSymbol("GET-INFO"), flag)) => GetInfo(parseInfoFlag(flag))
      case SList(List(SSymbol("EXIT"))) =>
        Exit
      case _ =>
        throw new UnknownCommand("Unknown command: " + cmd)
    }
    res
  }

  private def parseInfoFlag(flag: SExpr): InfoFlag = flag match {
    case SQualifiedSymbol(None, SSymbol("ERROR-BEHAVIOUR")) => ErrorBehaviourInfoFlag
    case SQualifiedSymbol(None, SSymbol("NAME")) => NameInfoFlag
    case SQualifiedSymbol(None, SSymbol("AUTHORS")) => AuthorsInfoFlag
    case SQualifiedSymbol(None, SSymbol("VERSION")) => VersionInfoFlag
    case SQualifiedSymbol(None, SSymbol("STATUS")) => StatusInfoFlag
    case SQualifiedSymbol(None, SSymbol("REASON-UNKNOWN")) => ReasonUnknownInfoFlag
    case SQualifiedSymbol(None, SSymbol("ALL-STATISTICS")) => AllStatisticsInfoFlag
    case SQualifiedSymbol(None, SSymbol(keyword)) => KeywordInfoFlag(keyword)
    case _ => sys.error("unexpected: " + flag + " when expecting info flag")
  }

  private def parseOption(option: List[SExpr]): SMTOption = option match {
    case List(SQualifiedSymbol(None, SSymbol("PRINT-SUCCESS")), SBool(bv)) => PrintSuccess(bv)
    case List(SQualifiedSymbol(None, SSymbol("VERBOSITY")), SInt(i)) => Verbosity(i.toInt)
    case List(SQualifiedSymbol(None, SSymbol("REGULAR-OUTPUT-CHANNEL")), SString(channel)) => RegularOutputChannel(channel)
    case List(SQualifiedSymbol(None, SSymbol("DIAGNOSTIC-OUTPUT-CHANNEL")), SString(channel)) => DiagnosticOutputChannel(channel)
    case _ => sys.error("unexpected: " + option + " when expecting option")
  }

  object SBool {
    def unapply(expr: SExpr): Option[Boolean] = expr match {
      case SSymbol("TRUE") => Some(true)
      case SSymbol("FALSE") => Some(false)
      case _ => None
    }
  }

  //todo: make sure no nested keyword in value
  private def parseAttribute(ss: List[SExpr]): Attribute = ss match {
    case List(SQualifiedSymbol(None, SSymbol(key))) => Attribute(key, None)
    case List(SQualifiedSymbol(None, SSymbol(key)), v) => Attribute(key, Some(v))
    case _ => sys.error("unexpected: " + ss + " when expecting attribute")
  }

}
