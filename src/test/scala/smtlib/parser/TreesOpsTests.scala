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

  test("Simple variable count exactly one variable") {
    assert(count(t => t == v1)(v1) === 1)
  }
  test("Simple variable does not count if different name") {
    assert(count(t => t == v2)(v1) === 0)
  }

}
