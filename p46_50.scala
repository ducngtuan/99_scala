/** Logic and Codes */

/** p46 */
def and(a: => Boolean, b: => Boolean)  = a && b
def or(a: => Boolean, b: => Boolean)   = a || b
def nand(a: => Boolean, b: => Boolean) = !(a && b)
def nor(a: => Boolean, b: => Boolean)  = !(a || b)
def xor(a: => Boolean, b: => Boolean)  = !equ(a, b)
def impl(a: => Boolean, b: => Boolean) = !a || (a && b)
def equ(a: => Boolean, b: => Boolean)  = (a && b) || (!a && !b)
def not(a: => Boolean) = !a

def table2(f: (Boolean, Boolean) => Boolean) {
  println("A     B     result")
  for (a <- List(true, false); b <- List(true, false)) {
    printf("%-5s %-5s %-5s\n", a, b, f(a, b))
  }
}

// table2((a, b) => and(a, or(a, b)))

/** p47 */
implicit def boolean2Decor(a: Boolean) = new {
  def and(b: => Boolean) = a && b
  def or(b: => Boolean)   = a || b
  def nand(b: => Boolean) = !(a && b)
  def nor(b: => Boolean)  = !(a || b)
  def xor(b: => Boolean)  = !equ(b)
  def impl(b: => Boolean) = !a || (a && b)
  def equ(b: => Boolean)  = (a && b) || (!a && !b)
}

// table2((a, b) => a and (a or not(b)))

/** p49 */
def gray(n: Int): List[String] =
  if (n == 0) List("")
  else {
    val lower = gray(n - 1)
    (lower map ("0" + _)) ::: (lower.reverse map ("1" + _))
  }

println(gray(3))

val grayBuff = collection.mutable.Map(0 -> List(""))
def gray2(n: Int): List[String] = {
  if (!grayBuff.contains(n)) {
    grayBuff += (n -> ((gray2(n-1) map ("0" + _)) ::: (gray2(n-1).reverse map ("1" + _))))
  }
  grayBuff(n)
}

println(gray2(3))