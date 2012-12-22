/** Extract a slice from a list */

def slice[A](start: Int, end: Int, ls: List[A]) = {
  @annotation.tailrec
  def _slice(start: Int, end: Int, ls: List[A], acc: List[A]): List[A] = (start, end, ls) match {
    case (_, 0, _) | (_, _, Nil) => acc.reverse
    case (0, end, x :: xs) => _slice(0, end - 1, xs, x :: acc)
    case _ => _slice(start - 1, end - 1, ls.tail, acc)
  }
  _slice(start, end, ls, Nil)
}

println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

// Built-in: list.slice(start, end)