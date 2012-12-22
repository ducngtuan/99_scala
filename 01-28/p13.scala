/** Run-length encoding of a list (direct solution) */

def encodeDirect[A](ls: List[A]) = {
  @annotation.tailrec
  def _encode(ls: List[A], acc: List[(Int, A)]): List[(Int, A)] = ls match {
    case Nil => acc.reverse
    case x :: xs =>
      val (packed, rest) = ls.span(_ == x)
      _encode(rest, (packed.length, x) :: acc)
  }
  _encode(ls, Nil)
}

println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))