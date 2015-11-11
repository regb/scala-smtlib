package smtlib
package theories

import parser.Terms._

private[theories] trait Operation1 {
  def name: String

  def apply(i : Term): Term =
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                          Seq(i))

  def unapply(t : Term): Option[Term] = t match {
    case FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(n), Seq()), None),
          Seq(i)) if n == name => Some(i)
    case _ => None
  }
}

private[theories] trait Operation2 {
  def name: String

  def apply(l : Term, r : Term): Term =
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                          Seq(l,r))

  def unapply(t : Term): Option[(Term, Term)] = t match {
    case FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(n), Seq()), None),
          Seq(l,r)) if n == name => Some((l,r))
    case _ => None
  }
}

private[theories] trait OperationN {
  def name: String

  def apply(is: Seq[Term]): Term =
    FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))), is)

  def unapply(t : Term): Option[Seq[Term]] = t match {
    case FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(n), Seq()), None),
          is) if n == name => Some(is)
    case _ => None
  }
}
