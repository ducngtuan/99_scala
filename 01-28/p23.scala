/** Extract a given number of randomly selected elements from a list */

def randomSelect[A](n: Int, ls: List[A]) = {
  def _rand(n: Int, ls: List[A], acc: List[A]): List[A] = (n, ls) match {
    case (0, _) | (_, Nil) => acc
    case (n, _) =>
      val (rest, e) = removeAt(util.Random.nextInt(ls.length), ls)
      _rand(n - 1, rest, e :: acc)
  }
  _rand(n, ls, Nil)
}

println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))

/** From p20 */
def removeAt[A](n: Int, ls: List[A]) = ls.splitAt(n) match {
  case (Nil, _) if n < 0 => throw new NoSuchElementException
  case (pre, e :: post) => (pre ::: post, e)
  case (pre, Nil) => throw new NoSuchElementException
}