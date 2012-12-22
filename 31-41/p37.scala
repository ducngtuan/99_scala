/** Calculate Euler's totient function phi(m) (improved) */
implicit def int2Algorithm(n: Int) = new Algorithm(n)

class Algorithm(val n: Int) {
  def totient: Int = 
    primeFactorMultiplicity.foldLeft(1) { case (t, (p, m)) => t * (p - 1) * math.pow(p, m - 1).toInt }

  /** From p36 */
  def primeFactorMultiplicity: Map[Int, Int] =
    primeFactors groupBy (x => x) mapValues (_.length)

  /** From p35 */
  def primeFactors: List[Int] = {
    def _pf(factor: Int, m: Int, acc: List[Int]): List[Int] =
      if (m == 1) acc.reverse
      else
        if (m % factor == 0) _pf(factor, m / factor, factor :: acc)
        else _pf(factor + 2, m, acc)

    if (n % 2 == 0) _pf(3, n / 2, List(2))
    else _pf(3, n, Nil)
  }
}

println(10.totient)