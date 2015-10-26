package smtlib
package theories
package experimental

import parser.Terms._

import Strings._
import Ints.NumeralLit

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class StringsTests extends FunSuite with ShouldMatchers {

  override def suiteName = "Strings theory test suite"

  test("String sort") {
    StringSort() match {
      case StringSort() => assert(true)
      case _ => assert(false)
    }

    StringSort() match {
      case FixedSizeBitVectors.BitVectorSort(n) if n == 14 => assert(false)
      case FixedSizeBitVectors.BitVectorSort(n) if n == 32 => assert(false)
      case Ints.IntSort() => assert(false)
      case Reals.RealSort() => assert(true)
      case StringSort() => assert(true)
      case _ => assert(false)
    }
  }

  test("literals") {
    val l1 = StringLit("abc")

    l1 match {
      case StringLit(n) => assert(n == "abc")
      case _ => assert(false)
    }
    
    val l2 = StringLit("")

    l2 match {
      case StringLit(n) => assert(n == "")
      case _ => assert(false)
    }
  }


  test("smtlib string format") {
    import parser.Parser
    
    implicit class TestParse(s: String) {
      def shouldParse(f: PartialFunction[Term, Any]) = {
        val term = Parser.fromString(s).parseTerm
        if(f.isDefinedAt(term)) f(term) else {
          error("Term " + s + " wrongly parsed as " + term)
        }
      }
      def shouldParseTo(p: Term) = {
        Parser.fromString(s).parseTerm should equal(p)
      }
    }
    
    
    "\"abc\"" shouldParseTo
    StringLit("abc")

    "(str.++ \"a\" \"bc\" )" shouldParseTo
    Concat(StringLit("a"), StringLit("bc"))

    "(str.++ \"a\" \"bc\" \"def\" )" shouldParseTo
    Concat(StringLit("a"), StringLit("bc"), StringLit("def"))
    
    "(str.len \"abcd\")" shouldParseTo
    Length(StringLit("abcd"))
    
    "(str.at \"abcd\" 1)" shouldParseTo
    At(StringLit("abcd"), NumeralLit(1))
    
    "(str.substr \"abcdef\" 2 5)" shouldParseTo
    Substring(StringLit("abcdef"), NumeralLit(2), NumeralLit(5))
  }
}
