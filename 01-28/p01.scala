/** Find the last element of a list */

def last[T](list: List[T]): T = list match {
  case x :: Nil => x
  case _ :: xs => last(xs)
  case _ => throw new NoSuchElementException()
}

println(last(List(1, 2, 3, 4)))
println(last("abcd".toList))

// build-in: List.last