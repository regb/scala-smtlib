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

  /*
   * The reason we need this general transformation framework
   * is because most of the trees have precise types, meaning
   * that a Term cannot appear where a Command is expected, and
   * that some parts have precise, non-compatible types, such
   * as attributes or keyword. It is thus not possible to define
   * a generic OperatorN deconstructor for Trees, if the goal is
   * to map the tree to a new, well-formed one. This object
   * defines the transform method for each of the type, and can
   * be overriden to match the exact mapping that is wanted by the
   * client.
   */

  /*
   * I need to figure out why we have an input context to the transformation
   * as it is currently not used (not depending on the return context). Maybe
   * it could be different type from the type returned, or maybe it
   * makes sense only when user override transform and modify the context
   * for the recursion (such as with a scope/symbol table)
   */

  /** Context computed while recursing down the trees */
  type C
  /** Result type computed at each tree */
  type R

  /** Combine the context with the results from subtrees
    *
    * combine is used after recursively transforming
    * the subtrees, to combine the multiple results into
    * one result to use in the post operation. The order
    * of the context is from left to right in the tree structure.
    *
    * Combine is always invoked, even on a single element sublist
    * or on an empty list. The tree argument is the current tree,
    * before applying the recursive transformation. Same for the
    * context argument, it is the current context at that transformation
    * step. So combine can be used to apply some function to each
    * tree element.
    */
  def combine(tree: Tree, context: C, results: Seq[R]): R

  def transform(term: Term, context: C): (Term, R) = {
    term match {
      case Let(vb, vbs, t) =>
        val (rvb, r1) = transform(vb, context)
        val (rvbs, r2s) = vbs.map(vb => transform(vb, context)).unzip
        val (rt, r3) = transform(t, context)
        (Let(rvb, rvbs, rt), combine(term, context, r1 +: r2s :+ r3))

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

  def transform(sort: Sort, context: C): (Sort, R) = {
    val Sort(id, subSorts) = sort
    val (rid, c) = transform(id, context)
    val (rsubSorts, cs) = subSorts.map(s => transform(s, context)).unzip
    (Sort(rid, rsubSorts), combine(sort, context, c +: cs))
  }

  def transform(id: Identifier, context: C): (Identifier, R) = {
    val (newIdx, cs) = id.indices.map(index => transform(index, context)).unzip
    (Identifier(id.symbol, newIdx), combine(id, context, cs))
  }


  def transform(vb: VarBinding, context: C): (VarBinding, R) = {
    val (nt, nc) = transform(vb.term, context)
    (VarBinding(vb.name, nt), nc)
  }
  def transform(sv: SortedVar, context: C): (SortedVar, R) = {
    val (ns, nc) = transform(sv.sort, context)
    (SortedVar(sv.name, ns), nc)
  }

  def transform(sexpr: SExpr, context: C): (SExpr, R) = ???

  //some tree elements need their own transform name, because they
  //are a subtype of existing trees so overload won't work, and they
  //still need their own specific transformation for rebuilding
  //the tree properly
  def transformSymbol(symbol: SSymbol, context: C): (SSymbol, R) = ???
  def transfromQualifiedIdentifier(qid: QualifiedIdentifier, context: C): (QualifiedIdentifier, R) = ???

  def transform(cmd: Command, context: C): (Command, R) = cmd match {
    case Assert(term) =>
      val (rt, rc) = transform(term, context)
      (Assert(rt), combine(cmd, context, Seq(rc)))

    case CheckSat() =>
      (CheckSat(), combine(cmd, context, Seq()))
    case CheckSatAssuming(propLiterals) =>
      val (rpls, resSyms) = propLiterals.map(pl => {
        val (ns, rs) = transformSymbol(pl.symbol, context)
        (PropLiteral(ns, pl.polarity), rs)
      }).unzip
      (CheckSatAssuming(rpls), combine(cmd, context, resSyms))

    case DeclareConst(name: SSymbol, sort: Sort) =>
      val (rn, rc1) = transformSymbol(name, context)
      val (rs, rc2) = transform(sort, context)
      (DeclareConst(rn, rs), combine(cmd, context, Seq(rc1, rc2)))

    case DeclareFun(name, paramSorts, returnSort) =>
      val (nameNew, nameRes) = transformSymbol(name, context)
      val (sortsNew, sortsRes) = paramSorts.map(s => transform(s, context)).unzip
      val (sortNew, sortRes) = transform(returnSort, context)
      val ndf = DeclareFun(nameNew, sortsNew, sortNew)
      (ndf, combine(cmd, context, nameRes +: sortsRes :+ sortRes))

    case DeclareSort(name, arity) =>
      val (nameNew, nameRes) = transformSymbol(name, context)
      (DeclareSort(nameNew, arity), combine(cmd, context, Seq(nameRes)))

  //case class DefineFun(funDef: FunDef) extends Command
  //case class DefineFunRec(funDef: FunDef) extends Command
  //case class DefineFunsRec(funDecls: Seq[FunDec], bodies: Seq[Term]) extends Command {
  //  require(funDecls.nonEmpty && funDecls.size == bodies.size)
  //}

    case DefineSort(name, params, sort) =>
      val (nameNew, nameRes) = transformSymbol(name, context)
      val (paramsNew, paramsRes) = params.map(s => transformSymbol(s, context)).unzip
      val (sortNew, sortRes) = transform(sort, context)
      val nds = DefineSort(nameNew, paramsNew, sortNew)
      (nds, combine(cmd, context, nameRes +: paramsRes :+ sortRes))

    case Echo(value: SString) =>
      (Echo(value), combine(cmd, context, Seq()))

    case Exit() => (cmd, combine(cmd, context, Seq()))

    case GetAssertions() => (cmd, combine(cmd, context, Seq()))
    case GetAssignment() => (cmd, combine(cmd, context, Seq()))

  //case class GetInfo(flag: InfoFlag) extends Command
  //case class GetModel() extends Command
  //case class GetOption(key: SKeyword) extends Command
  //case class GetProof() extends Command
  //case class GetUnsatAssumptions() extends Command
  //case class GetUnsatCore() extends Command
  //case class GetValue(term: Term, terms: Seq[Term]) extends Command

  ////TODO: should n be an SNumeral so that we can have a Position?
  //case class Pop(n: Int) extends Command
  //case class Push(n: Int) extends Command

  //case class Reset() extends Command
  //case class ResetAssertions() extends Command

  //case class SetInfo(attribute: Attribute) extends Command
  //case class SetLogic(logic: Logic) extends Command
  //case class SetOption(option: SMTOption) extends Command

    case _ => ???

  ////non standard declare-datatypes (no support for parametric types)
  //case class DeclareDatatypes(datatypes: Seq[(SSymbol, Seq[Constructor])]) extends Command

  }

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
    * The term will be mapped
    * before performing the recursion in the subtrees. This
    * should only do the mapping, without any recursive
    * call.
    */
  def pre(term: Term, context: C): (Term, C)

  /** a post-transformation to be applied to the term
    *
    * The term will be mapped
    * after performing the recursion in the subtrees. This
    * should only do the mapping, without any recursive
    * call.
    */
  def post(term: Term, result: R): (Term, R)

  final override def transform(term: Term, context: C): (Term, R) = {
    val (preTerm, preContext) = pre(term, context)
    val (postTerm, postContext) = super.transform(preTerm, preContext)
    post(postTerm, postContext)
  }

  def pre(sort: Sort, context: C): (Sort, C)
  def post(sort: Sort, result: R): (Sort, R)
  final override def transform(sort: Sort, context: C): (Sort, R) = {
    val (preSort, preContext) = pre(sort, context)
    val (postSort, postResult) = super.transform(preSort, preContext)
    post(postSort, postResult)
  }

}

/** a tree transformer without carrying a context */
abstract class SimpleTreeTransformer extends PrePostTreeTransformer {
  type C = Unit
  type R = Unit

  def pre(term: Term): Term
  def post(term: Term): Term
  def pre(sort: Sort): Sort
  def post(sort: Sort): Sort

  final override def combine(tree: Tree, c: C, cs: Seq[R]): R = ()

  final override def pre(term: Term, c: C): (Term, C) = (pre(term), ())
  final override def post(term: Term, r: R): (Term, R) = (post(term), ())
  final override def pre(sort: Sort, c: C): (Sort, C) = (pre(sort), ())
  final override def post(sort: Sort, c: R): (Sort, R) = (post(sort), ())

  final def transform(term: Term): Term = transform(term, ())._1
  final def transform(sort: Sort): Sort = transform(sort, ())._1

  override final def transform(id: Identifier, c: C): (Identifier, R) = transform(id, c)
  final def transform(id: Identifier): Identifier = transform(id, ())._1

  override final def transform(varBinding: VarBinding, c: C): (VarBinding, R) = transform(varBinding, c)
  final def transform(varBinding: VarBinding): VarBinding = transform(varBinding, ())._1

  override final def transform(sortedVar: SortedVar, c: C): (SortedVar, R) = transform(sortedVar, c)
  final def transform(sortedVar: SortedVar): SortedVar = transform(sortedVar, ())._1

}

abstract class TreeFolder extends TreeTransformer {

  type C = Unit

  def combine(tree: Tree, accs: Seq[R]): R
  override final def combine(tree: Tree, context: Unit, results: Seq[R]): R = {
    combine(tree, results)
  }
  //  require(cs.nonEmpty)
  //  if(cs.size == 1) cs.head else cs.reduce((c1, c2) => combine(c1, c2))
  //}

  final def fold(tree: Tree): R = tree match {
    case (term: Term) => transform(term, ())._2
    case (s: Sort) => transform(s, ())._2
    case (id: Identifier) => transform(id, ())._2
    case (vb: VarBinding) => transform(vb, ())._2
    case (sv: SortedVar) => transform(sv, ())._2
    case (cmd: Command) => transform(cmd, ())._2
    case (cmd: CommandResponse) => ???
    case (attr: Attribute) => ???
    case (flag: InfoFlag) => ???
    case (opt: SMTOption) => ???
    case (e: SExpr) => transform(e, ())._2
  }
}

abstract class TreeTraverser extends PrePostTreeTransformer {

  type C = Unit
  type R = Unit

  def pre(term: Tree): Unit
  def post(term: Tree): Unit

  override final def combine(tree: Tree, c: Unit, cs: Seq[Unit]): Unit = ()

  override final def pre(term: Term, c: C): (Term, C) = {
    pre(term)
    (term, ())
  }
  override final def post(term: Term, r: R): (Term, R) = {
    post(term)
    (term, ())
  }
  override final def pre(sort: Sort, c: C): (Sort, C) = {
    pre(sort)
    (sort, ())
  }
  override final def post(sort: Sort, r: R): (Sort, R) = {
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
