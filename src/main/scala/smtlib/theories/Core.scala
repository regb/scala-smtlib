package smtlib
package theories

import parser.Terms._

object Core {


  object BoolSort {

    def apply(): Sort = Sort(Identifier(SSymbol("Bool")))

    def unapply(sort: Sort): Boolean = sort match {
      case Sort(Identifier(SSymbol("Bool"), Seq()), Seq()) => true
      case _ => false
    }

  }

  object True {

    def apply(): Term = QualifiedIdentifier(Identifier(SSymbol("true")))
    
    def unapply(term: Term): Boolean = term match {
      case QualifiedIdentifier(Identifier(SSymbol("true"), Seq()), None) => true
      case _ => false
    }

  }

  object False {

    def apply(): Term = QualifiedIdentifier(Identifier(SSymbol("false")))
    
    def unapply(term: Term): Boolean = term match {
      case QualifiedIdentifier(Identifier(SSymbol("false"), Seq()), None) => true
      case _ => false
    }

  }


  object Not {

    def apply(t: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("not"))), Seq(t))
    
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("not"), Seq()),
          None
        ), Seq(t)) => Some(t)
      case _ => None
    }
  }

  object Implies {
    
    def apply(t1: Term, t2: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("=>"))), Seq(t1, t2))
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("=>"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object And {
    
    def apply(t1: Term, t2: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("and"))), Seq(t1, t2))
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("and"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Or {
    
    def apply(t1: Term, t2: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("or"))), Seq(t1, t2))
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("or"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }


  object Xor {
    
    def apply(t1: Term, t2: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("xor"))), Seq(t1, t2))
    
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("xor"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

}
