/** Goldbach's conjecture */

implicit def int2Algorithm(n: Int) = new Algorithm(n)

class Algorithm(val n: Int) {
  def goldbach =
    primes takeWhile (_ < n) find (p => (n - p).isPrime) match {
      case None => throw new IllegalArgumentException
      case Some(p) => (p, n - p)
    }
    

  /** From p31 */
  def isPrime = primes takeWhile (_ <= math.sqrt(n)) forall (n % _ != 0)
  val primes: Stream[Int] = 2 #:: (Stream.from(3, 2) filter (_.isPrime))
}

println(28.goldbach)