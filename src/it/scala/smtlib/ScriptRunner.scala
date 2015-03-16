package smtlib

import scala.sys.process._

import org.scalatest.FunSuite

import java.io.File
import java.io.FileReader

import parser.Parser
import lexer.Lexer
import printer.RecursivePrinter
import interpreters._

class ScriptRunner extends FunSuite {

  
  def isZ3Available: Boolean = try {
    val output: String = "z3 -help".!! 
    true
  } catch {
    case (_: Exception) => false
  }
  
  def isCVC4Available: Boolean = try {
    val output: String = "cvc4".!!
    true
  } catch {
    case (e: Exception) => {
      false
    }
  }


  def run(file: File): Unit = {
    if(isZ3Available) {
      val output: Stream[String] = Seq("z3", "-smt2", file.getPath).lineStream

      val l = new Lexer(new FileReader(file))
      val p = new Parser(l)
      val z3Interpreter = new Z3Interpreter("z3")

      output.foreach((expected: String) => {
        val res: String = z3Interpreter.eval(p.parseCommand).toString
        assert(expected.trim === res.trim)
      })
    }

    if(isCVC4Available) {

    }
  }

}

class Tests extends FunSuite {

  val all: String => Boolean = (s: String) => true
  val resourceDirHard = "src/it/resources/"

  def filesInResourceDir(dir : String, filter : String=>Boolean = all) : Iterable[File] = {    
    import scala.collection.JavaConversions._
    val d = this.getClass.getClassLoader.getResource(dir)
    val asFile = if(d == null || d.getProtocol != "file") {
      // We are in Eclipse. The only way we are saved is by hard-coding the path               
      new File(resourceDirHard + dir)
    } else new File(d.toURI())
    asFile.listFiles().filter(f => filter(f.getPath()))
  }

  def mkTest(file: File)(block: => Unit) = {

    test("SMTLIB benchmark: " + file.getPath) {
      (new ScriptRunner).run(file)
    }

  }

  filesInResourceDir("regression/smtlib/solving", _.endsWith(".smt2")).foreach(file =>
    mkTest(file) {}
  )

}

