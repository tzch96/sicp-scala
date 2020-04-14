// Exercise 1.28:
// (Re-worded) Implement the Miller-Rabin primality test using the expmod procedure which signals if it discovers a nontrivial square root of 1 mod n.
// A nontrivial square root of 1 mod n is a number not equal to 1 or n - 1 whose square is equal to 1 modulo n.

def expmod(base: Int, exp: Int, m: Int): Int = {
  def checkNontrivialSqrt(n: Int, m: Int): Int = {
    val x: Int = (n * n) % m

    if (n != 1 && n != (m - 1) && x == 1) 0 else x
  }

  if (exp == 0) 1
  else if (exp % 2 == 0)
    checkNontrivialSqrt(expmod(base, exp / 2, m), m)
  else
    (base * expmod(base, exp - 1, m)) % m
}

def millerRabinTest(n: Int, a: Int): Boolean = {
  expmod(a, n - 1, n) == 1
}

def isPrime(n: Int): Boolean = {
  if (n == 2) true
  else if (n % 2 == 0) false
  else checkPrime(n, n - 1)
}

def checkPrime(n: Int, a: Int): Boolean = {
  if (a == 0) true
  else if (millerRabinTest(n, a)) millerRabinTest(n, a - 1)
  else false
}

// test
println(isPrime(2))
println(isPrime(3))
println(isPrime(101))
println(isPrime(109))
println(isPrime(6601))
println(isPrime(2821))
