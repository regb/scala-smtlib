package smtlib.parser

import Terms._
import Commands._
import CommandsResponses._

/** Transformer for any tree
  *
  * This is the most general transformer for trees, it
  * can be extended for the exact implementation needed by
  * overriding the context type C and the pre/post methods
  * for transforming the tree before and after the traversal
  * for each node.
  */
abstract class TreeTransformer {

  type C

  /** Combine the contexts from subtrees
    *
    * combine is used after recursively transforming
    * the subtrees, to combine the multiple contexts into
    * one context to use in the post operation. The order
    * of the context is from left to right in the tree structure.
    *
    * Combine is always invoked, even on a single element sublist
    * or on an empty list. The tree argument is the current tree,
    * before applying the recursive transformation. Same for the
    * context argument, it is the current context at that transformation
    * step.
    */
  def combine(tree: Tree, context: C, contexts: Seq[C]): C

  def transform(term: Term, context: C): (Term, C) = {
    term match {
      case Let(vb, vbs, t) =>
        val (rvb, c1) = transform(vb, context)
        val (rvbs, c2s) = vbs.map(vb => transform(vb, context)).unzip
        val (rt, c3) = transform(t, context)
        (Let(rvb, rvbs, rt), combine(term, context, c1 +: c2s :+ c3))

      case Forall(sv, svs, t) =>
        val (rsv, c1) = transform(sv, context)
        val (rsvs, c2s) = svs.map(sv => transform(sv, context)).unzip
        val (rt, c3) = transform(t, context)
        (Forall(rsv, rsvs, rt), combine(term, context, c1 +: c2s :+ c3))
      case Exists(sv, svs, t) =>
        val (rsv, c1) = transform(sv, context)
        val (rsvs, c2s) = svs.map(sv => transform(sv, context)).unzip
        val (rt, c3) = transform(t, context)
        (Exists(rsv, rsvs, rt), combine(term, context, c1 +: c2s :+ c3))

      case AnnotatedTerm(t, attribute, attributes) =>
        val (rt, rc) = transform(t, context)
        (AnnotatedTerm(rt, attribute, attributes), combine(term, context, Seq(rc)))

      case FunctionApplication(fun, ts) =>
        //TODO: how to transform the qualified identifier properly?
        val (rts, cs) = ts.map(t => transform(t, context)).unzip
        (FunctionApplication(fun, rts), combine(term, context, cs))

      case (cst: Constant) => (cst, combine(cst, context, Seq()))

      case QualifiedIdentifier(id, sort) =>
        val (rid, c1) = transform(id, context)
        val rsort = sort.map(s => transform(s, context))
        (QualifiedIdentifier(rid, rsort.map(_._1)), 
         combine(term, context, c1 +: rsort.map(_._2).toSeq))

      case (ext: TermExtension) => ???
    }
  }

  def transform(sort: Sort, context: C): (Sort, C) = {
    val Sort(id, subSorts) = sort
    val (rid, c) = transform(id, context)
    val (rsubSorts, cs) = subSorts.map(s => transform(s, context)).unzip
    (Sort(rid, rsubSorts), combine(sort, context, c +: cs))
  }

  def transform(id: Identifier, context: C): (Identifier, C) = {
    val (newIdx, cs) = id.indices.map(index => transform(index, context)).unzip
    (Identifier(id.symbol, newIdx), combine(id, context, cs))
  }


  def transform(vb: VarBinding, context: C): (VarBinding, C) = {
    val (nt, nc) = transform(vb.term, context)
    (VarBinding(vb.name, nt), nc)
  }
  def transform(sv: SortedVar, context: C): (SortedVar, C) = {
    val (ns, nc) = transform(sv.sort, context)
    (SortedVar(sv.name, ns), nc)
  }

  def transform(sexpr: SExpr, context: C): (SExpr, C) = ???

}

/** A tree transformer with pre/post transformation
  *
  * This extends the generic TreeTransformer by adding pre and
  * post transformation to be applied before and after the
  * recursive transformations. The core transform becomes final,
  * as it cannot be extended easily due to the wrapping of pre/post
  * around it. Most basic transformation will work by implementing
  * either of pre or post.
  */
abstract class PrePostTreeTransformer extends TreeTransformer {

  /** a pre-transformation to be applied to the term
    *
    * If defined (default to None), the term will be mapped
    * before performing the recursion in the subtrees. This
    * should only do the mapping, without any recursive
    * call.
    */
  def pre(term: Term, context: C): (Term, C)

  /** a post-transformation to be applied to the term
    *
    * If defined (default to None), the term will be mapped
    * after performing the recursion in the subtrees. This
    * should only do the mapping, without any recursive
    * call.
    */
  def post(term: Term, context: C): (Term, C)

  final override def transform(term: Term, context: C): (Term, C) = {
    val (preTerm, preContext) = pre(term, context)
    val (postTerm, postContext) = super.transform(preTerm, preContext)
    post(postTerm, postContext)
  }

  def pre(sort: Sort, context: C): (Sort, C)
  def post(sort: Sort, context: C): (Sort, C)
  final override def transform(sort: Sort, context: C): (Sort, C) = {
    val (preSort, preContext) = pre(sort, context)
    val (postSort, postContext) = super.transform(preSort, preContext)
    post(postSort, postContext)
  }

}

/** a tree transformer without carrying a context */
abstract class SimpleTreeTransformer extends PrePostTreeTransformer {
  type C = Unit

  def pre(term: Term): Term
  def post(term: Term): Term
  def pre(sort: Sort): Sort
  def post(sort: Sort): Sort

  final override def combine(tree: Tree, c: C, cs: Seq[C]): C = ()

  final override def pre(term: Term, c: C): (Term, C) = (pre(term), ())
  final override def post(term: Term, c: C): (Term, C) = (post(term), ())
  final override def pre(sort: Sort, c: C): (Sort, C) = (pre(sort), ())
  final override def post(sort: Sort, c: C): (Sort, C) = (post(sort), ())

  final def transform(term: Term): Term = transform(term, ())._1
  final def transform(sort: Sort): Sort = transform(sort, ())._1

  override final def transform(id: Identifier, c: C): (Identifier, C) = transform(id, c)
  final def transform(id: Identifier): Identifier = transform(id, ())._1

  override final def transform(varBinding: VarBinding, c: C): (VarBinding, C) = transform(varBinding, c)
  final def transform(varBinding: VarBinding): VarBinding = transform(varBinding, ())._1

  override final def transform(sortedVar: SortedVar, c: C): (SortedVar, C) = transform(sortedVar, c)
  final def transform(sortedVar: SortedVar): SortedVar = transform(sortedVar, ())._1

}

abstract class TreeFolder extends TreeTransformer {

  //def combine(acc: C, t: Tree): C
  //override final def combine(tree: Tree, cs: Seq[C]): C = {
  //  require(cs.nonEmpty)
  //  if(cs.size == 1) cs.head else cs.reduce((c1, c2) => combine(c1, c2))
  //}

  final def fold(tree: Tree, c: C): C = tree match {
    case (term: Term) => transform(term, c)._2
    case (s: Sort) => transform(s, c)._2
    case (id: Identifier) => transform(id, c)._2
    case (vb: VarBinding) => transform(vb, c)._2
    case (sv: SortedVar) => transform(sv, c)._2
    case (cmd: Command) => ???
    case (cmd: CommandResponse) => ???
    case (attr: Attribute) => ???
    case (flag: InfoFlag) => ???
    case (opt: SMTOption) => ???
    case (e: SExpr) => transform(e, c)._2
  }
}

abstract class TreeTraverser extends PrePostTreeTransformer {

  type C = Unit

  def pre(term: Tree): Unit
  def post(term: Tree): Unit

  override final def combine(tree: Tree, c: Unit, cs: Seq[Unit]): Unit = ()

  override final def pre(term: Term, c: C): (Term, C) = {
    pre(term)
    (term, ())
  }
  override final def post(term: Term, c: C): (Term, C) = {
    post(term)
    (term, ())
  }
  override final def pre(sort: Sort, c: C): (Sort, C) = {
    pre(sort)
    (sort, ())
  }
  override final def post(sort: Sort, c: C): (Sort, C) = {
    post(sort)
    (sort, ())
  }

  final def traverse(tree: Tree): Unit = tree match {
    case (t: Term) => transform(t, ())
    case (s: Sort) => transform(s, ())
    case (id: Identifier) => transform(id, ())
    case (vb: VarBinding) => transform(vb, ())
    case (sv: SortedVar) => transform(sv, ())
    case (cmd: Command) => ???
    case (cmd: CommandResponse) => ???
    case (attr: Attribute) => ???
    case (flag: InfoFlag) => ???
    case (opt: SMTOption) => ???
    case (e: SExpr) => transform(e, ())
  }

}
