package smtlib
package it

import scala.sys.process._

import org.scalatest.FunSuite

import java.io.File
import java.io.FileReader

import parser.Parser
import lexer.Lexer
import printer.RecursivePrinter
import interpreters._

class ScriptRunner extends FunSuite {


  //def run(file: File): Unit = {
  //  if(isZ3Available) {

  //    val l = new Lexer(new FileReader(file))
  //    val p = new Parser(l)
  //    val z3Interpreter = new Z3Interpreter("z3")

  //    output.foreach((expected: String) => {
  //      val res: String = z3Interpreter.eval(p.parseCommand).toString
  //      assert(expected.trim === res.trim)
  //    })
  //  }

  //  if(isCVC4Available) {

  //    val l = new Lexer(new FileReader(file))
  //    val p = new Parser(l)
  //    val cvc4Interpreter = new CVC4Interpreter("cvc4")

  //    output.foreach((expected: String) => {
  //      val res: String = cvc4Interpreter.eval(p.parseCommand).toString
  //      assert(expected.trim === res.trim)
  //    })

  //  }
  //}

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

//  def mkTest(file: File)(block: => Unit) = {
//
//    if(isZ3Available) {
//      test("SMTLIB benchmark: " + file.getPath) {
//        (new ScriptRunner).run(file)
//      }
//    }
//
//    if(isCVC4Available) {
//
//    }
//
//  }
//
  filesInResourceDir("regression/smtlib/solving/all", _.endsWith(".smt2")).foreach(file => {
    if(isZ3Available) {
      test("With Z3: SMTLIB benchmark: " + file.getPath) {
        compareWithInterpreter(executeZ3)(getZ3Interpreter, file)
      }
    }
    if(isCVC4Available) {
      test("With CVC4: SMTLIB benchmark: " + file.getPath) {
        compareWithInterpreter(executeCVC4)(getCVC4Interpreter, file)
      }
    }
  })

  if(isZ3Available) {
    filesInResourceDir("regression/smtlib/solving/z3", _.endsWith(".smt2")).foreach(file => {
      test("With Z3: SMTLIB benchmark: " + file.getPath) {
        compareWithInterpreter(executeZ3)(getZ3Interpreter, file)
      }
    })
  }

  if(isCVC4Available) {
    filesInResourceDir("regression/smtlib/solving/cvc4", _.endsWith(".smt2")).foreach(file => {
      test("With CVC4: SMTLIB benchmark: " + file.getPath) {
        compareWithInterpreter(executeCVC4)(getCVC4Interpreter, file)
      }
    })
  }

  
  def getZ3Interpreter: Interpreter = Z3Interpreter.buildDefault
  def getCVC4Interpreter: Interpreter = CVC4Interpreter.buildDefault

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

  def executeZ3(file: File): Stream[String] = {
    val output: Stream[String] = Seq("z3", "-smt2", file.getPath).lineStream
    output
  }

  def executeCVC4(file: File): Stream[String] = {
    val output: Stream[String] = Seq("cvc4", "--lang", "smt", file.getPath).lineStream
    output
  }

  def compareWithInterpreter(executable: (File) => Stream[String])
                            (interpreter: Interpreter, file: File) = {

    println("running solver on: " + file.getPath)
    val output = executable(file)

    val l = new Lexer(new FileReader(file))
    val p = new Parser(l)

    output.foreach((expected: String) => {
      val res: String = interpreter.eval(p.parseCommand).toString
      assert(expected.trim === res.trim)
    })
    assert(p.parseCommand === null)
  }

}

