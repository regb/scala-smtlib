package smtlib
package theories

import parser.Terms._

object Reals {


  object RealSort {

    def apply(): Sort = {
      Sort(Identifier(SSymbol("Real")))
    }

    def unapply(sort: Sort): Boolean = sort match {
      case Sort(Identifier(SSymbol("Real"), Seq()), Seq()) => true
      case _ => false
    }

  }

  object NumeralLit {

    def apply(value: BigInt): Term = SNumeral(value)
    
    def unapply(term: Term): Option[BigInt] = term match {
      case SNumeral(value) => Some(value)
      case _ => None
    }

  }
  
  object DecimalLit {

    def apply(value: BigDecimal): Term = SDecimal(value)
    
    def unapply(term: Term): Option[BigDecimal] = term match {
      case SDecimal(value) => Some(value)
      case _ => None
    }

  }


  object Neg {

    def apply(t: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("-"))), Seq(t))
    
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("-"), Seq()),
          None
        ), Seq(t)) => Some(t)
      case _ => None
    }
  }

  object Add {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("+"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("+"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Sub {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("-"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("-"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }


  object Mul {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("*"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("*"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Div {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("div"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("div"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object LessThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("<"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("<"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object LessEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("<="))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("<="), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object GreaterThan {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(">"))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(">"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object GreaterEquals {

    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(">="))),
        Seq(t1, t2)
      )
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(">="), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

}
