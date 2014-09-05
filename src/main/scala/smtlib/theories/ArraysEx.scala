package smtlib
package theories

import parser.Terms._

object ArraysEx {


  object ArraySort {

    def apply(from: Sort, to: Sort): Sort = Sort(Identifier(SSymbol("Array")), Seq(from, to))

    def unapply(sort: Sort): Option[(Sort, Sort)] = sort match {
      case Sort(Identifier(SSymbol("Array"), Seq()), Seq(from, to)) => Some((from, to))
      case _ => None
    }

  }

  object Select {
    
    def apply(t1: Term, t2: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("select"))), Seq(t1, t2))

    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("select"), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }

  }

  object Store {
    
    def apply(t1: Term, t2: Term, t3: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("store"))), Seq(t1, t2, t3))

    def unapply(term: Term): Option[(Term, Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol("term"), Seq()),
          None
        ), Seq(t1, t2, t3)) => Some((t1, t2, t3))
      case _ => None
    }

  }

}
