/** Rotate a list N places to the left */

def rotate[A](n: Int, ls: List[A]): List[A] =
  if (n < 0) rotate(ls.length + n, ls)
  else {
    val n0 = if (ls.isEmpty) 0 else n % ls.length
    val (pre, rest) = ls.splitAt(n0)
    rest ::: pre
  }

println(rotate(1, Nil))
println(rotate(0, Nil))
println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
// res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
// res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)