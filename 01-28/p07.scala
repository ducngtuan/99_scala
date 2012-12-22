/** Flatten a nested list structure */

def flatten(list: List[Any]): List[Any] = list flatMap {
  case l : List[_] => flatten(l)
  case x => List(x)
}

println(flatten(List(List(1, 1), 2, List(List(3)))))