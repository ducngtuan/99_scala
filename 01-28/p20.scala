/** Remove the Kth element from a list */

def removeAt[A](n: Int, ls: List[A]) = ls.splitAt(n) match {
  case (Nil, _) if n < 0 => throw new NoSuchElementException
  case (pre, e :: post) => (pre ::: post, e)
  case (pre, Nil) => throw new NoSuchElementException
}

println(removeAt(1, List('a, 'b, 'c, 'd)))