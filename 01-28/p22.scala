/** Create a list from a given range */

def range(start: Int, end: Int) = List.range(start, end + 1)

def range2(start: Int, end: Int) = {
  @annotation.tailrec
  def _range(n: Int, acc: List[Int]): List[Int] =
    if (n < start) acc
    else _range(n - 1, n :: acc)

  _range(end, Nil)
}

println(range(4, 9))
println(range2(4, 9))