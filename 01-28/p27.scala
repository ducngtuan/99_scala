/** Group the elements of a set into disjoint subsets */

def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
  case Nil => List(Nil)
  case n :: ns =>
    ls.combinations(n).toList flatMap (c => group(ns, ls filterNot (c contains)) map (c :: _))
}

println(group(List(1, 2, 2), List('a, 'b, 'c, 'd, 'e)))