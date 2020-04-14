// Exercise 1.24:
// Modify the timed-prime-test procedure from the previous exercises to use fast-prime (the Fermat method).

def smallestDivisor(n: Long): Long = findDivisor(n, 2)

def next(n: Long): Long = if (n == 2) 3 else n + 2

def findDivisor(n: Long, test: Long): Long = {
  if (test * test > n) n
  else if (test % n == 0) test
  else findDivisor(n, next(n))
}

def power(base: Long, exp: Long, m: Long): Long = {
  if (exp == 0) 1
  else if (exp % 2 == 1) {
    (power(base, exp / 2, m) * power(base, exp / 2, m)) / m
  }
  else (power(base, exp - 1, m) * power(base, exp - 1, m)) / m
}

def fermat(n: Long): Boolean = {
  def check(a: Long): Boolean = {
    power(a, n, n) == a
  }

  check(1 + scala.util.Random.between(0, n - 1))
}

def fastPrime(n: Long, times: Int): Boolean = {
  if (times == 0) true
  else if (fermat(n)) fastPrime(n, times - 1)
  else false
}

def isPrime(n: Long): Boolean = {
  fastPrime(n, 100)
}

def timedPrimeTest(n: Long): Double = {
  val t0 = System.nanoTime()
  if (isPrime(n)) {
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
