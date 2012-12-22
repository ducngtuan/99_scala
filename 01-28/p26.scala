/** Generate the combinations of K distinct objects chosen from the N elements of a list */

def flatMapSubs[A, B](ls: List[A])(f: List[A] => List[B]): List[B] = ls match {
  case Nil => Nil
  case _ => f(ls) ::: flatMapSubs(ls.tail)(f)
}
/**
 * Comb(k)(1, 2, 3, 4...) = prepend 1 to each of Comb(k-1)(2, 3, 4...) ++ Comb(k)(2, 3, 4...)
 * flatMapSubs(1, 2, 3, 4...)(f) = f(1, 2, 3, 4...) ++ flatMapSubs(2, 3, 4...)(f)
 * --> Comb(k)(1, 2, 3, 4...) = flatMapSubs(1, 2, 3, 4...)(Comb(k))
 */
def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
  if (n == 0) List(Nil)
  else flatMapSubs(ls) { sl => combinations(n - 1, sl.tail) map (sl.head :: _) }
}

//flatMapSubs(List('a, 'b, 'c, 'd)){ sl => println(sl); sl }
println(combinations(2, List('a, 'b, 'c, 'd)))