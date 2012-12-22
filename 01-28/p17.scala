/** Split a list into two parts */

def split[A](n: Int, ls: List[A]) = {
  @annotation.tailrec
  def _split(n: Int, rest: List[A], pre: List[A]): (List[A], List[A]) = (n, rest) match {
    case (_, Nil) | (0, _) => (pre.reverse, rest)
    case (_, x :: xs) => _split(n - 1, xs, x :: pre)
  }
  _split(n, ls, Nil)
}

println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

// Built-in: list.splitAt(n)