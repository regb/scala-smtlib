package smtlib
package theories

import Ints._

import org.scalatest.FunSuite

class IntsTests extends FunSuite {

  override def suiteName = "Ints theory test suite"

  test("Int sort") {
    IntSort() match {
      case IntSort() => assert(true)
      case _ => assert(false)
    }

    IntSort() match {
      case FixedSizeBitVectors.BitVectorSort(14) => assert(false)
      case IntSort() => assert(true)
      case _ => assert(false)
    }
  }

  test("literals") {
    val l1 = NumeralLit(42)

    l1 match {
      case NumeralLit(n) => assert(n == 42)
      case _ => assert(false)
    }

  }


  test("smtlib format") {
    import parser.Parser

    Parser.fromString("12").parseTerm match {
      case NumeralLit(n) => assert(n == 12)
      case _ => assert(false)
    }

    //Parser.fromString("(concat #b101 #b01)").parseTerm match {
    //  case Concat(
    //        BitVectorLit(List(true, false, true)),
    //        BitVectorLit(List(false, true))
    //       ) => assert(true)
    //  case _ => assert(false)
    //}
    //Parser.fromString("((_ extract 1 2) #b101)").parseTerm match {
    //  case Extract(1, 2, BitVectorLit(List(true, false, true))) => assert(true)
    //  case _ => assert(false)
    //}

    //Parser.fromString("(bvand #b101 #b011)").parseTerm match {
    //  case And(
    //        BitVectorLit(List(true, false, true)),
    //        BitVectorLit(List(false, true, true))
    //       ) => assert(true)
    //  case _ => assert(false)
    //}

    //Parser.fromString("(bvor #b101 #b011)").parseTerm match {
    //  case Or(
    //        BitVectorLit(List(true, false, true)),
    //        BitVectorLit(List(false, true, true))
    //       ) => assert(true)
    //  case _ => assert(false)
    //}

    //Parser.fromString("(bvadd #b101 #b011)").parseTerm match {
    //  case Add(
    //        BitVectorLit(List(true, false, true)),
    //        BitVectorLit(List(false, true, true))
    //       ) => assert(true)
    //  case _ => assert(false)
    //}

    //Parser.fromString("(bvmul #b101 #b011)").parseTerm match {
    //  case Mul(
    //        BitVectorLit(List(true, false, true)),
    //        BitVectorLit(List(false, true, true))
    //       ) => assert(true)
    //  case _ => assert(false)
    //}

  }
}
