/** Binary trees */
sealed abstract class Tree[+T] {
  def isSymmetric = {
    def isMirrorOf[A, B](l: Tree[A], r: Tree[B]): Boolean = (l, r) match {
      case (End, End) => true
      case (Node(_, l1, r1), Node(_, l2, r2)) => isMirrorOf(l1, r2) && isMirrorOf(r1, l2)
      case _ => false
    }
    this match {
      case End => true
      case Node(_, l, r) => isMirrorOf(l, r)
    }
  }

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = this match {
    case End => Node(x)
    case Node(value, l, r) => if (value < x) Node(value, l, r.addValue(x)) else Node(value, l.addValue(x), r)
  }

  def nodeCount: Int = this match {
    case End => 0
    case Node(_, l, r) => 1 + l.nodeCount + r.nodeCount
  }
  
  def leafCount: Int = this match {
    case Node(_, End, End) => 1
    case Node(_, l, r) => l.leafCount + r.leafCount
    case _ => 0
  }
  
  def leafList: List[T] = this match {
    case End => List()
    case Node(value, End, End) => List(value)
    case Node(_, l, r) => l.leafList ::: r.leafList
  }
  
  def internalList: List[T] = this match {
    case End | Node(_, End, End) => List()
    case Node(value, l, r) => value :: l.internalList ::: r.internalList
  }
  
  def atLevel(n: Int): List[T] = (n, this) match {
    case (1, Node(value, _, _)) => List(value)
    case (n, Node(_, l, r)) => l.atLevel(n - 1) ::: r.atLevel(n - 1)
    case _ => Nil
  }
  
  def layoutBinaryTree: Tree[T] = _layout(1, 1)._1
  private def _layout(x: Int, depth: Int): (Tree[T], Int) = this match {
    case End => (End, x)
    case Node(v, l, r) =>
      val (ll, xl) = l._layout(x, depth + 1)
      val (lr, xr) = r._layout(xl + 1, depth + 1)
      (PositionedNode(v, ll, lr, xl, depth), xr)
    case _ => (this, x)
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](n: Int, value: T): List[Tree[T]] =
    if (n < 1) List(End)
    else if (n % 2 == 1) {
      val subs = cBalanced(n / 2, value)
      subs flatMap (l => subs map (r => Node(value, l, r)))
    } else {
      val less = cBalanced((n-1) / 2, value)
      val more = cBalanced((n-1) / 2 + 1, value)
      less flatMap (l => more flatMap (r => List(Node(value, l, r), Node(value, r, l))))
    }

  def fromList[T <% Ordered[T]](ls: List[T]) = ls.foldLeft(End: Tree[T])((t, e) => t addValue e)

  def symmetricBalancedTrees[T](n: Int, value: T) =
    cBalanced(n, value) filter (_.isSymmetric)

  def hbalTrees[T](height: Int, value: T): List[Tree[T]] =
    if (height < 1) List(End)
    else if (height == 1) List(Node(value))
    else {
      val full = hbalTrees(height-1, value)
      val short = hbalTrees(height-2, value)
      (full flatMap (l => full map (r => Node(value, l, r)))) :::
        (full flatMap (f => short flatMap (s => List(Node(value, f, s), Node(value, s, f)))))
    }

  def minHbalNodes(height: Int): Int =
    if (height < 1) 0
    else if (height == 1) 1
    else minHbalNodes(height - 1) + minHbalNodes(height-2) + 1

  def maxHbalNodes(height: Int) = height * 2 - 1

  def minHbalHeight(n: Int): Int =
    if (n == 0) 0
    else minHbalHeight(n / 2) + 1

  def maxHbalHeight(n: Int) =
    Stream.from(1) takeWhile (minHbalNodes(_) <= n) last

  def hbalTreesWithNodes[T](n: Int, value: T) =
    (minHbalHeight(n) to maxHbalHeight(n)) flatMap (hbalTrees(_, value)) filter (_.nodeCount == n) toList

  def completeBinaryTree[T](n: Int, value: T) = {
    def generate(m: Int): Tree[T] =
      if (m > n) End
      else Node(value, generate(2 * m), generate(2 * m + 1))
    generate(1)
  }
}

// println(Tree.cBalanced(4, "x"))
// println(Node('a', Node('b'), Node('c')).isSymmetric)
// println(Tree.fromList(List(3, 2, 5, 7, 1)))
// println(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric) // true
// println(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric) // false
// println(Tree.symmetricBalancedTrees(5, "x"))
// println(Tree.hbalTrees(3, "x"))
// println(Tree.hbalTreesWithNodes(15, "x").length)
// println(Node("x", Node("x"), End).leafCount)
// println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList)
// println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
// println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
// println(Tree.completeBinaryTree(6, "x"))
println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)
println(Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree)
//println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2)