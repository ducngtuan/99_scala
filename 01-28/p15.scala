/** Duplicate the elements of a list a given number of times */

def duplicateN[A](n: Int, ls: List[A]) =
  ls flatMap { x => List.fill(n)(x) }

println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))