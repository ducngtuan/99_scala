/** Lotto: Draw N different random numbers from the set 1..M */
def lotto(n: Int, m: Int) = randomSelect(n, List.range(1, m + 1))

println(lotto(6, 49))

/** From p23 */
def randomSelect[A](n: Int, ls: List[A]) = {
  def _rand(n: Int, ls: List[A], acc: List[A]): List[A] = (n, ls) match {
    case (0, _) | (_, Nil) => acc
    case (n, _) =>
      val (rest, e) = removeAt(util.Random.nextInt(ls.length), ls)
      _rand(n - 1, rest, e :: acc)
  }
  _rand(n, ls, Nil)
}

/** From p20 */
def removeAt[A](n: Int, ls: List[A]) = ls.splitAt(n) match {
  case (Nil, _) if n < 0 => throw new NoSuchElementException
  case (pre, e :: post) => (pre ::: post, e)
  case (pre, Nil) => throw new NoSuchElementException
}