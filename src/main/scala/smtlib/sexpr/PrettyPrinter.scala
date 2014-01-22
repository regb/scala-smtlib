package smtlib.sexpr

import SExprs._

import java.io.Writer
import java.io.StringWriter

object PrettyPrinter {

  def apply(sexpr: SExpr, writer: Writer): Unit = sexpr match {
    case SList(sexprs) => ppNary(writer, sexprs, "(", " ", ")")
    case SString(s) => {
      writer.append('"')
      writer.write(s)
      writer.append('"')
    }
    case SSymbol(s) => writer.write(s)
    case SQualifiedSymbol(os, s) => {
      os.foreach(apply(_, writer))
      writer.append(':')
      writer.write(apply(s))
    }
    case SInt(i) => writer.write(i.toString)
    case SDouble(d) => writer.write(d.toString)
    case SComment(s) => {
      writer.append(';')
      writer.write(s)
      writer.append('\n')
    }
  }

  def apply(sexpr: SExpr): String = {
    val sWriter = new StringWriter
    apply(sexpr, sWriter)
    sWriter.toString
  }

  private def ppNary(writer: Writer, exprs: Seq[SExpr], pre: String, op: String, post: String): Unit = {
    writer.write(pre)
    var c = 0
    var sz = exprs.size

    exprs.foreach(e => {
      apply(e, writer)
      if(c < sz) writer.write(op)
    })
    writer.write(post)
  }

}
