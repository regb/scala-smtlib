package smtlib.parser


/** Generic and useful operation on entire trees
  *
  * This contains most traversal operations that can be
  * simply described as applying some function over the whole
  * tree. Mappings are more complex as they need to return
  * new trees, and would usually be defined as a full tree transformer
  * as it needs to return different type for each exact tree.
  */
object TreesOps {

  def count(p: (Tree) => Boolean)(t: Tree): Int = {
    val folder = new TreeFolder {
      type C = Int
      override def combine(node: Tree, c: Int, counts: Seq[Int]) = 
        counts.sum + (if(p(node)) 1 else 0)
    }
    folder.fold(t, 0)
  }

  //TODO: how to implement fold without a starting value ?
  //def fold[T](f: (Tree, Seq[T]) => T)(t: Tree): T = {
  //  val folder = new TreeFolder {
  //    type C = T
  //    override def combine(node: Tree, c: T, cs: Seq[T]): T = f(node, cs)
  //  }
  //  folder.fold(t, ???)
  //}


  def foreach(f: (Tree) => Unit)(t: Tree): Unit = {
    val traverser = new TreeTraverser {
      override def pre(tree: Tree): Unit = f(tree)
      override def post(tree: Tree): Unit = ()
    }
    traverser.traverse(t)
  }

}
