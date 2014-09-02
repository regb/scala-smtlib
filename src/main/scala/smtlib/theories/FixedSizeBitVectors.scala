package smtlib
package theories

import parser.Terms._

import common._

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

    def apply(content: Hexadecimal): Term = SHexadecimal(content)
    
    def unapply(term: Term): Option[List[Boolean]] = term match {
      case SBinary(content) => Some(content)
      case SHexadecimal(hexa) => Some(hexa.toBinary)
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

  object Extract {

    def apply(i: Int, j: Int, t: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("extract"), Seq(i, j))),
        Seq(t)
      )
    
    def unapply(term: Term): Option[(Int, Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("extract"), Seq(i, j)),
          None
        ), Seq(t)) => Some((i, j, t))
      case _ => None
    }

  }

  object And {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvand"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvand"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Or {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvor"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvor"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Not {

    def apply(t: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("bvnot"))), Seq(t))
    
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvnot"), Seq()),
          None
        ), Seq(t)) => Some(t)
      case _ => None
    }
  }


  object Neg {

    def apply(t: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("bvneg"))), Seq(t))
    
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvneg"), Seq()),
          None
        ), Seq(t)) => Some(t)
      case _ => None
    }
  }


  object Add {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvadd"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvadd"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }


  object Mul {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvmul"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvmul"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object UDiv {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvudiv"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvudiv"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object URem {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvurem"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvurem"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object ULessThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvult"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvult"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SignedLessThan {

    private def firstBit(t: Term) = Extract(31, 31, t)
    private def lastBits(t: Term) = Extract(30, 0, t)

    def apply(t1: Term, t2: Term): Term =
      Core.Or(
        ULessThan(firstBit(t2), firstBit(t1)),
        Core.And(
          Core.Equals(firstBit(t1), firstBit(t2)),
          Core.Or(
            Core.And(
              Core.Equals(firstBit(t1), BitVectorLit(List(true))),
              ULessThan(Neg(t2), Neg(t1))
            ),
            Core.And(
              Core.Equals(firstBit(t1), BitVectorLit(List(false))),
              ULessThan(t1, t2)
            )
          )
        )
      )
      
  }
}
