// Exercise 1.22:
// (Re-worded) Write a procedure that searches for primes that checks the primality of consecutive odd integers in a specified range.
// Use the procedure to find three smallest primes > 1 000, three > 10 000 (note: should probably increase the numbers for more modern hardware).
// Note the time needed to test each prime.

def smallestDivisor(n: Long): Long = findDivisor(n, 2)

def findDivisor(n: Long, test: Long): Long = {
  if (test * test > n) n
  else if (test % n == 0) test
  else findDivisor(n, test + 1)
}

def timedPrimeTest(n: Long): Double = {
  val t0 = System.nanoTime()
  if (n == smallestDivisor(n)) {
    val t1 = System.nanoTime()
    print(s"$n ${t1 - t0}\n")
  }
  val t1 = System.nanoTime()
  t1 - t0
}

def primesIn(lower: Long, upper: Long) = {
  def iter(n: Long): Double = {
    if (n <= upper)
      timedPrimeTest(n)
    else iter(n + 2)
  }

  if (lower % 2 == 1) iter(lower) else iter(lower + 1)
}

primesIn(1000, 1019)
