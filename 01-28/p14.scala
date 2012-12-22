/** Duplicate the elements of a list */

def duplicate[A](ls: List[A]) =
  ls flatMap { x => List(x, x) }

println(duplicate(List('a, 'b, 'c, 'c, 'd)))