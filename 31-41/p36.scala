/** Determine the prime factors of a given positive integer */
implicit def int2Algorithm(n: Int) = new Algorithm(n)

class Algorithm(val n: Int) {
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

println(315.primeFactors)
println(315.primeFactorMultiplicity)