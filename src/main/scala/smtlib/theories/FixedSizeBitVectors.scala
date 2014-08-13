package smtlib
package theories

import parser.Terms._

object FixedSizeBitVectors {


  object BitVectorSort {

    def apply(length: Int): Sort = {
      require(length > 0)
      Sort(Identifier(SSymbol("BitVec"), Seq(length)))
    }

    def unapply(sort: Sort): Option[Int] = sort match {
      case Sort(Identifier(SSymbol("BitVec"), Seq(n)), Seq()) if n > 0 => Some(n)
      case _ => None
    }

  }

  object BitVectorLit {

    def apply(content: List[Boolean]): Term = SBinary(content)
    
    def unapply(term: Term): Option[List[Boolean]] = term match {
      case SBinary(content) => Some(content)
      case SHexaDecimal(hexa) => Some(hexa.toBinary)
      case _ => None
    }

  }


  object Concat {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("concat"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("concat"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

}
