/** Find the Kth element of a list */
@annotation.tailrec
def nth[T](n: Int, list: List[T]): T =
  if (list.isEmpty) throw new NoSuchElementException
  else
    if (n == 0) list.head
    else nth(n - 1, list.tail)

println(nth(1, List(1, 2, 3, 4)))
println(nth(2, "abcd".toList))

// build-in: list(n)