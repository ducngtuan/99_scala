/** Determine whether two positive integer numbers are coprime */

implicit def coprimeTo(n: Int) = new {
  def isCoprimeTo(m: Int) = gcd(n, m) == 1

  /** From p32 */
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)
}

println(35.isCoprimeTo(64))