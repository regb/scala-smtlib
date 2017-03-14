package smtlib
package parser

import org.scalatest.FunSuite

import Terms._
import Commands._
import TreesOps._

class TreesOpsTests extends FunSuite {

  val s1 = Sort(Identifier(SSymbol("S1")))
  val s2 = Sort(Identifier(SSymbol("S2")))
  val s3 = Sort(Identifier(SSymbol("S3")), Seq(s1, s2))

  val v1 = QualifiedIdentifier(Identifier(SSymbol("v1")))
  val v2 = QualifiedIdentifier(Identifier(SSymbol("v2")))
  val v3 = QualifiedIdentifier(Identifier(SSymbol("v3")))

  val f1 = QualifiedIdentifier(Identifier(SSymbol("f1")))
  val f2 = QualifiedIdentifier(Identifier(SSymbol("f2")))
  val f3 = QualifiedIdentifier(Identifier(SSymbol("f3")))

  test("count function is 1 if exactly one variable") {
    assert(count(t => t == v1)(v1) === 1)
  }
  test("count function is 0 if variable has different name") {
    assert(count(t => t == v2)(v1) === 0)
  }
  test("count function finds and count variable just once") {
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(v1))) === 1)
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(v1, v2))) === 1)
    assert(count(t => t == v2)(FunctionApplication(f1, Seq(v1, v2))) === 1)
  }
  test("count function finds and count variable several times") {
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(v1))) === 1)
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(v1, v1))) === 2)
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(v1, v2, v1))) === 2)
    assert(count(t => t == v1)(FunctionApplication(f1, Seq(FunctionApplication(f2, Seq(v1, v2)), v1, v2, v1))) === 3)
  }

  test("count function finds and count variable in commands") {
    assert(count(t => t == v1)(Assert(FunctionApplication(f1, Seq(v1)))) === 1)
    assert(count(t => t == v1)(Assert(FunctionApplication(f1, Seq(FunctionApplication(f2, Seq(v1, v2)), v1, v2, v1)))) === 3)
  }

}
