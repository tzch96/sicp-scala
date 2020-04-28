// Exercise 1.33:
// You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion
// of a filter on the terms to be combined. That is, combine only those terms derived from values in the range
// that satisfy a specified condition.
//
// The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with
// an additional predicate of one argument that specifies the filter.
//
// Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
//   a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime?
//      predicate already written)
//   b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive
//      integers i < n such that GCD(i, n) = 1).

// from ex1-32
def accumulate(combiner: (Int, Int) => Int, nullValue: Int, term: Int => Int, a: Int, next: Int => Int, b: Int): Int = {
  if (a > b) nullValue
  else combiner(term(a), accumulate(combiner, nullValue, term, next(a), next, b))
}

def filteredAccumulate(filter: Int => Boolean, combiner: (Int, Int) => Int, nullValue: Int, term: Int => Int, a: Int, next: Int => Int, b: Int): Int = {
  def filteredTerm(x: Int) = if (filter(x)) term(x) else nullValue

  accumulate(combiner, nullValue, filteredTerm, a, next, b)
}

// a. sum of the squares of primes between a and b
def isPrime(n: Int): Boolean = {
  if (n <= 1) false
  else if (n == 2) true
  else !(2 to (n - 1)).exists(x => n % x == 0)
}

def sum(a: Int, b: Int) = filteredAccumulate(isPrime, (x: Int, y: Int) => x + y, 0, (x: Int) => x * x, a, (x: Int) => x + 1, b)

// b. product of i < n such that GCD(i, n) = 1
def gcd(m: Int, n: Int): Int = {
  if (m < n) gcd(n, m)
  else if (n == 0) m
  else gcd(n, m % n)
}

def isRelativelyPrime(m: Int, n: Int) = gcd(m, n) == 1

def product(n: Int) = {
  def filter(m: Int) = isRelativelyPrime(m, n)

  filteredAccumulate(filter, (n: Int, m: Int) => n * m, 1, (n: Int) => n, 1, (n: Int) => n + 1, n)
}

// test
println(s"a. sum(0, 10) = ${sum(0, 10)}")
println(s"b. product(10) = ${product(10)}")
