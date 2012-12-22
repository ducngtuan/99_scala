/** Modified run-length encoding */

def encodeModified[A](ls: List[A]) =
  encode(ls) map { t => if (t._1 == 1) t._2 else t }

def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
  encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }

println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
println(encodeModified2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

/** From p10 */
def encode[A](ls: List[A]): List[(Int, A)] = pack(ls) map (xs => (xs.length, xs.head))

/** From p09 */
def pack[A](ls: List[A]): List[List[A]] = {
  @annotation.tailrec
  def _pack(ls: List[A], acc: List[List[A]]): List[List[A]] = ls match {
    case Nil => acc.reverse
    case x :: xs =>
      val (l, r) = ls.span(_ == x)
      _pack(r, l :: acc)
  }
  _pack(ls, Nil)
}