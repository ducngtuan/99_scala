/** Find the number of elements of a list */

def length[T](list: List[T]): Int = list match {
  case Nil => 0
  case _ :: xs => 1 + length(xs)
}

def length2[T](list: List[T]): Int = list.foldLeft(0) { (count, _) => count + 1 }

def length3[T](list: List[T]): Int = {
  @annotation.tailrec
  def _length(list: List[T], acc: Int): Int = list match {
    case Nil => acc
    case _ :: xs => _length(xs, acc + 1)
  }
  _length(list, 0)
}

println(length(List(1, 2, 3, 4)))
println(length2("abcd".toList))
println(length3("abcde".toList))

// build-in: list.length