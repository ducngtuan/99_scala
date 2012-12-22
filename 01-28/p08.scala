/** Eliminate consecutive duplicates of list elements */

def compress[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case _ :: Nil => list
  case x :: y :: xs => if (x == y) compress(list.tail) else x :: compress(list.tail)
}

def compress2[A](list: List[A]): List[A] = {
  def _compress(list: List[A], acc: List[A]): List[A] = list match {
    case Nil => acc.reverse
    case x :: xs => if (x == acc.head) _compress(xs, acc) else _compress(xs, x :: acc)
  }
  list match {
    case Nil => Nil
    case x :: xs => _compress(xs, List(x))
  }
}

def compress3[A](list: List[A]): List[A] =
  list.foldRight(List[A]()) { (x, acc) => if (acc.isEmpty || x != acc.head) x :: acc else acc }

println(compress(List('a, 'a, 'b, 'c, 'c, 'c)))
println(compress2(List('a, 'a, 'b, 'c, 'c, 'c)))
println(compress3(List('a, 'a, 'b, 'c, 'c, 'c)))