/** Decode a run-length encoded list */

def decode[A](ls: List[(Int, A)]): List[A] =
  ls flatMap (t => List.fill(t._1)(t._2))

println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))