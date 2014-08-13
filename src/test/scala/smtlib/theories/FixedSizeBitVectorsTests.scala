package smtlib
package theories

import Core._
import FixedSizeBitVectors._

import org.scalatest.FunSuite

class FixedSizeBitVectorsTests extends FunSuite {

  override def suiteName = "Bit Vector theory test suite"

  test("BitVector sort") {
    BitVectorSort(32) match {
      case BitVectorSort(14) => assert(false)
      case BitVectorSort(32) => assert(true)
      case _ => assert(false)
    }

    BitVectorSort(12) match {
      case BitVectorSort(14) => assert(false)
      case BitVectorSort(32) => assert(false)
      case BitVectorSort(12) => assert(true)
      case _ => assert(false)
    }
  }

  test("literals") {
    val l1 = BitVectorLit(List(true, true, false))

    l1 match {
      case BitVectorLit(List(true, false, true)) => assert(false)
      case BitVectorLit(List(true, true, false, false)) => assert(false)
      case BitVectorLit(List(true, true, false)) => assert(true)
      case _ => assert(false)
    }

  }


  //test("smtlib format") {
  //  import parser.Parser

  //  Parser.fromString("true").parseTerm match {
  //    case True() => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("false").parseTerm match {
  //    case False() => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("(not true)").parseTerm match {
  //    case False() => assert(false)
  //    case True() => assert(false)
  //    case Not(True()) => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("(or true false)").parseTerm match {
  //    case False() => assert(false)
  //    case True() => assert(false)
  //    case Not(True()) => assert(false)
  //    case And(True(), False()) => assert(false)
  //    case Or(True(), True()) => assert(false)
  //    case Or(True(), False()) => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("(and true false)").parseTerm match {
  //    case False() => assert(false)
  //    case True() => assert(false)
  //    case Not(True()) => assert(false)
  //    case Or(True(), False()) => assert(false)
  //    case And(True(), True()) => assert(false)
  //    case And(True(), False()) => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("(=> true false)").parseTerm match {
  //    case False() => assert(false)
  //    case True() => assert(false)
  //    case Not(True()) => assert(false)
  //    case Or(True(), False()) => assert(false)
  //    case Implies(True(), True()) => assert(false)
  //    case Implies(True(), False()) => assert(true)
  //    case _ => assert(false)
  //  }

  //  Parser.fromString("(xor true false)").parseTerm match {
  //    case False() => assert(false)
  //    case True() => assert(false)
  //    case Not(True()) => assert(false)
  //    case Or(True(), False()) => assert(false)
  //    case Xor(True(), True()) => assert(false)
  //    case Xor(True(), False()) => assert(true)
  //    case _ => assert(false)
  //  }
  //}

}
