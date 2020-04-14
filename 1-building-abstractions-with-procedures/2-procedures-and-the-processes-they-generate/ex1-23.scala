// Exercise 1.23:
// Modify the smallest-divisor procedure from 1.22 to use (next test-divisor) instead of (+ test-divisor 1). Next returns 3 if its input is equal to 2 and input + 2 otherwise. Observe the difference in speed.

def smallestDivisor(n: Long): Long = findDivisor(n, 2)

def next(n: Long): Long = if (n == 2) 3 else n + 2

def findDivisor(n: Long, test: Long): Long = {
  if (test * test > n) n
  else if (test % n == 0) test
  else findDivisor(n, next(n))
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
