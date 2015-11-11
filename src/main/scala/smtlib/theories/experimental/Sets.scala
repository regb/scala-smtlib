package smtlib
package theories
package experimental
import parser.Terms._

/* Experimental support for the theory of sets in CVC4
 * Based on the operations in http://cvc4.cs.nyu.edu/wiki/Sets
 */
object Sets {
  object SetSort {
    def apply(el : Sort): Sort = Sort(Identifier(SSymbol("Set")), Seq(el))

    def unapply(sort : Sort): Option[Sort] = sort match {
      case Sort(Identifier(SSymbol("Set"), Seq()), Seq(el)) => Some(el)
      case _ => None
    }
  }

  object Union extends Operation2 { override val name = "union" }
  object Intersection extends Operation2 { override val name = "intersection" }
  object Setminus extends Operation2 { override val name = "setminus" }
  object Member extends Operation2 { override val name = "member" }
  object Subset extends Operation2 { override val name = "subset" }

  object EmptySet {
    def apply(s : Sort): Term = QualifiedIdentifier(Identifier(SSymbol("emptyset")), Some(s))
    def unapply(t : Term): Option[Sort] = t match {
      case QualifiedIdentifier(Identifier(SSymbol("emptyset"), Seq()), Some(s)) =>
          Some(s)
      case _ => None
    }
  }

  object Singleton extends Operation1 { override val name = "singleton" }

  object Insert extends OperationN { override val name = "insert" }
}
