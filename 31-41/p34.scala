/**
 * Calculate Euler's totient function phi(m).
 * Euler's so-called totient function phi(m) is defined as the number of 
 * positive integers r (1 <= r <= m) that are coprime to m.
 */
implicit def int2Algorithm(n: Int) = new Algorithm(n)

class Algorithm(val n: Int) {
  def totient() = (1 to n) filter (_.isCoprimeTo(n)) length

  /** From p33 */
  def isCoprimeTo(m: Int) = gcd(n, m) == 1

  /** From p32 */
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)  
}

println(10.totient)