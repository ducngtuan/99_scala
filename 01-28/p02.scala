/** Find the last but one element of a list */

def penultimate[T](list: List[T]): T = list match {
  case x :: _ :: Nil => x
  case _ :: xs => penultimate(xs)
  case _ => throw new NoSuchElementException()
}

println(penultimate(List(1, 2, 3, 4)))
println(penultimate("abcd".toList))

// build-in: List.init.last