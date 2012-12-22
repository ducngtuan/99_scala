/** Generate a random permutation of a list */

def randomPermute[A](ls: List[A]) =
  randomSelect(ls.length, ls)

// Use shuffle
def randomPermute2[A: Manifest](ls: List[A]) = {
  val a = ls.toArray
  var i = a.length - 1
  while (i > 0) {
    val j = util.Random.nextInt(i + 1)
    val t = a(j)
    a(j) = a(i)
    a(i) = t
    i -= 1
  }
  a.toList
}

println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
println(randomPermute2(List('a, 'b, 'c, 'd, 'e, 'f)))

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