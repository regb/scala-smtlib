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
    
    //TODO: going from List[Boolean] to Integer
    def unapply(term: Term): Option[List[Boolean]] = term match {
      case SBinary(content) => Some(content)
      case SHexadecimal(hexa) => Some(hexa.toBinary)
      case _ => None
    }

  }

  //shorthand notation (_ bv13 32) for the number 13 with 32 bits
  object BitVectorConstant {

    def apply(x: BigInt, n: Int): Term =
      QualifiedIdentifier(Identifier(SSymbol("bv" + x), Seq(n)))
    
    def unapply(term: Term): Option[(BigInt, Int)] = term match {
      case QualifiedIdentifier(Identifier(SSymbol(name), Seq(n)), None) => {
        if(name.startsWith("bv")) {
          val value = name.substring(2)
          scala.util.Try(BigInt(value).toInt).toOption.map(v => (v, n))
        } else None
      }
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

  object Repeat {

    def apply(i: Int, t: Term): Term = {
      require(i >= 1)
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("repeat"), Seq(i))),
        Seq(t)
      )
    }
    
    def unapply(term: Term): Option[(Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("repeat"), Seq(i)),
          None
        ), Seq(t)) => Some((i, t))
      case _ => None
    }

  }

  object ZeroExtend {

    def apply(i: Int, t: Term): Term = {
      require(i >= 0)
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("zero_extend"), Seq(i))),
        Seq(t)
      )
    }
    
    def unapply(term: Term): Option[(Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("zero_extend"), Seq(i)),
          None
        ), Seq(t)) => Some((i, t))
      case _ => None
    }

  }

  object SignExtend {

    def apply(i: Int, t: Term): Term = {
      require(i >= 0)
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("sign_extend"), Seq(i))),
        Seq(t)
      )
    }
    
    def unapply(term: Term): Option[(Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("sign_extend"), Seq(i)),
          None
        ), Seq(t)) => Some((i, t))
      case _ => None
    }

  }

  object RotateLeft {

    def apply(i: Int, t: Term): Term = {
      require(i >= 0)
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("rotate_left"), Seq(i))),
        Seq(t)
      )
    }
    
    def unapply(term: Term): Option[(Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("rotate_left"), Seq(i)),
          None
        ), Seq(t)) => Some((i, t))
      case _ => None
    }

  }

  object RotateRight {

    def apply(i: Int, t: Term): Term = {
      require(i >= 0)
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("rotate_right"), Seq(i))),
        Seq(t)
      )
    }
    
    def unapply(term: Term): Option[(Int, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("rotate_right"), Seq(i)),
          None
        ), Seq(t)) => Some((i, t))
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

  object NAnd {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvnand"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvnand"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object NOr {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvnor"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvnor"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object XOr {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvxor"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvxor"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object XNOr {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvxnor"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvxnor"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Comp {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvcomp"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvcomp"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
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

  object Sub {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsub"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsub"), Seq()),
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

  object SDiv {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsdiv"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsdiv"), Seq()),
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

  object SRem {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsrem"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsrem"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SMod {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsmod"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsmod"), Seq()),
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

  object ULessEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvule"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvule"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object UGreaterThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvugt"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvugt"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object UGreaterEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvuge"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvuge"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SLessThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvslt"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvslt"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SLessEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsle"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsle"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SGreaterThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsgt"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsgt"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object SGreaterEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvsge"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvsge"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object ShiftLeft {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvshl"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvshl"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object LShiftRight {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvlshr"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvlshr"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object AShiftRight {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("bvashr"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("bvashr"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

}
