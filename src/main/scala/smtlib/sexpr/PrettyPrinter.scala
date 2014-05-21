package smtlib.sexpr

import SExprs._

import java.io.Writer
import java.io.StringWriter

/*
 * Symbols are printed with quotations (|SYMBOL|) because this is the way you are supposed to escape 
 * lower cases in Common Lisp. However, existing solvers (at the time of commenting --- January 22 
 * 2014 --- Z3 and CVC4 at least) are not respecting such things as they are invalidly parsing the
 * input: <a> and <A> as different symbols while they should be both internally represented as <A>
 * Printing with the quotation seem to make the printer useable by other SMT solvers while still
 * respecting the standard.
 *
 * TODO: Double vs BigDecimals ?
 */

object PrettyPrinter {

  def apply(sexpr: SExpr, writer: Writer, smtLibCompatibility: Boolean = false): Unit = sexpr match {
    case SList(sexprs) => ppNary(writer, sexprs, "(", " ", ")")
    case SString(s) => {
      writer.append('"')
      writer.write(s)
      writer.append('"')
    }
    case SSymbol(s) if(smtLibCompatibility) => {
      writer.write(s)
    }
    case SSymbol(s) => {
      writer.write("|")
      writer.write(s)
      writer.write("|")
    }
    case SQualifiedSymbol(os, s) => {
      os.foreach(apply(_, writer, smtLibCompatibility))
      writer.append(':')
      apply(s, writer, smtLibCompatibility)
    }
    case SInt(i) => writer.write(i.toString)
    case SDouble(d) => writer.write(d.toString)
    case SComment(s) => {
      writer.append(';')
      writer.write(s)
      writer.append('\n')
    }
  }

  def toString(sexpr: SExpr): String = {
    val sWriter = new StringWriter
    apply(sexpr, sWriter, smtLibCompatibility)
    sWriter.toString
  }

  private def ppNary(writer: Writer, exprs: Seq[SExpr], pre: String, op: String, post: String): Unit = {
    writer.write(pre)
    var c = 0
    var sz = exprs.size

    exprs.foreach(e => {
      apply(e, writer, smtLibCompatibility)
      c += 1
      if(c < sz) writer.write(op)
    })
    writer.write(post)
  }

}
