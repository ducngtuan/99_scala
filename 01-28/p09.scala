/** Pack consecutive duplicates of list elements into sublists */

def pack[A](ls: List[A]): List[List[A]] = {
  @annotation.tailrec
  def _pack(ls: List[A], acc: List[List[A]]): List[List[A]] = ls match {
    case Nil => acc.reverse
    case x :: xs =>
      val (l, r) = ls.span(_ == x)
      _pack(r, l :: acc)
  }
  _pack(ls, Nil)
}

println(pack(Nil))
println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))