/** Drop every Nth element from a list */

def drop[A](n: Int, ls: List[A]) = {
  def _drop(m: Int, ls: List[A], acc: List[A]): List[A] =
    (m, ls) match {
      case (_, Nil) => acc.reverse
      case (1, x :: xs) => _drop(n, xs, acc)
      case (m, x :: xs) => _drop(m - 1, xs, x :: acc)
    }
  _drop(n, ls, Nil)
}

def drop2[A](n: Int, ls: List[A]) =
  ls.zipWithIndex filter (e => (e._2 + 1) % n != 0) map (_._1)

println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
println(drop2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))