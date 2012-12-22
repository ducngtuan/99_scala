def isPrime(n: Int) = primes takeWhile (_ <= math.sqrt(n)) forall (n % _ != 0)

val primes: Stream[Int] =
  2 #:: (Stream.from(3, 2) filter isPrime)

println(isPrime(7))
println(isPrime(11))
println(isPrime(10))