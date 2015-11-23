package smtlib
package theories

import parser.Terms._

object Common {
  /**
   * Operations with no arguments
   */
  trait Operation0 {
    val name: String

    def apply(): Term =
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                            Seq())

    def unapply(t : Term): Boolean = t match {
      case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(`name`), Seq()), None),
            Seq()) => true
      case _ => false
    }
  }

  /**
   * Operations with exactly one argument
   */
  trait Operation1 {
    val name: String

    def apply(i : Term): Term =
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                            Seq(i))

    def unapply(t : Term): Option[Term] = t match {
      case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(`name`), Seq()), None),
            Seq(i)) => Some(i)
      case _ => None
    }
  }

  /**
   * Operations with exactly two argument
   */
  trait Operation2 {
    val name: String

    def apply(l : Term, r : Term): Term =
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                            Seq(l,r))

    def unapply(t : Term): Option[(Term, Term)] = t match {
      case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(`name`), Seq()), None),
            Seq(l,r)) => Some((l,r))
      case _ => None
    }
  }

  /**
   * Operations with exactly three argument
   */
  trait Operation3 {
    val name: String

    def apply(l : Term, m : Term, r : Term): Term =
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))),
                            Seq(l,m,r))

    def unapply(t : Term): Option[(Term, Term, Term)] = t match {
      case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(`name`), Seq()), None),
            Seq(l,r)) => Some((l,m,r))
      case _ => None
    }
  }

  /**
   * Operations with variable number of arguments, requiring that the number of arguments is greater than least `numRequired`
   */
  trait OperationN {
    val name: String

    val numRequired: Int

    def apply(is: Seq[Term]): Term = {
      require(is.size >= numRequired)
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(name))), is)
    }

    def unapply(t : Term): Option[Seq[Term]] = t match {
      case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(`name`), Seq()), None),
            is) if is.size >= numRequired => Some(is)
      case _ => None
    }
  }

  /**
   * Operations with variable number of arguments, none required
   */
  trait OperationN0 extends OperationN {
    override val numRequired: Int = 0

    def apply(is: Term*): Term = apply(is)
  }

  /**
   * Operations with variable number of arguments, at least one required
   */
  trait OperationN1 {
    override val numRequired: Int = 1

    def apply(i1: Term, is: Term*): Term = apply(i1 +: is)
  }

  /**
   * Operations with variable number of arguments, at least two required
   */
  trait OperationN2 {
    override val numRequired: Int = 2

    def apply(i1: Term, i2: Term, is: Term*): Term = apply(i1 +: i2 +: is)
  }

  /**
   * Operations with variable number of arguments, at least three required
   */
  trait OperationN3 {
    override val numRequired: Int = 3

    def apply(i1: Term, i2: Term, i3: Term, is: Term*): Term = apply(i1 +: i2 +: i3 +: is)
  }
}
