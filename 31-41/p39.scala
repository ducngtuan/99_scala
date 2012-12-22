/** List of prime numbers */

/** From p31 */
def isPrime(n: Int) = primes takeWhile (_ <= math.sqrt(n)) forall (n % _ != 0)
val primes: Stream[Int] = 2 #:: (Stream.from(3, 2) filter isPrime)

def listPrimesInRange(r: Range) =
  primes dropWhile (_ < r.head) takeWhile (_ <= r.last) toList

println(listPrimesInRange(7 to 31))