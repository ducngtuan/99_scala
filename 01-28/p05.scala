/** Reverse a list */

def reverse[A](list: List[A]): List[A] = list match {
  case Nil => list
  case x :: xs => reverse(xs) ::: List(x)
}

def reverse2[A](list: List[A]): List[A] = {
  @annotation.tailrec
  def _reverse(list: List[A], acc: List[A]): List[A] = list match {
    case Nil => acc
    case x :: xs => _reverse(xs, x :: acc)
  }
  _reverse(list, Nil)  
}

def reverse3[A](list: List[A]): List[A] =
  list.foldLeft(List[A]()) { (result, item) => item :: result }

println(reverse(List(1, 2, 3, 4)))
println(reverse2(List(1, 2, 3, 4)))
println(reverse3(List(1, 2, 3, 4)))

// Built-in: list.reverse