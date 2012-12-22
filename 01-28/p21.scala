/** Insert an element at a given position into a list */

def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
  val (pre, post) = ls.splitAt(n)
  pre ::: e :: post
}

println(insertAt('new, 1, List('a, 'b, 'c, 'd)))