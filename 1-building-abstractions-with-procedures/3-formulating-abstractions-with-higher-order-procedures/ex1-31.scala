// Exercise 1.31:
// The sum procedure is only the simplest of a vast number of similar abstractions that can be captured
// as higher-order procedures. Write an analogous procedure called product that returns the product of
// the values of a function at points over a given range.
//
// Show how to define factorial in terms of product.
//
// Also use product to compute approximations to pi using the formula:
//  pi / 4 = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)
//
// If your product procedure generates a recursive process, write one that generates an iterative process.
// If it generates an iterative process, write one that generates a recursive process.

def product(term: Double => Double, a: Double, next: Double => Double, b: Double): Double = {
  if (a > b) 1
  else term(a) * product(term, next(a), next, b)
}

def factorial(n: Double) = product((x: Double) => x, 1, (x: Double) => x + 1, n)

def piTerm(n: Double) = {
  if (n % 2 == 0) ((n + 2) / (n + 1))
  else ((n + 1) / (n + 2))
}

def pi(n: Double) = 4 * product(piTerm, 1, (x: Double) => x + 1, n)

def productIter(term: Double => Double, a: Double, next: Double => Double, b: Double) = {
  def iter(a: Double, result: Double): Double = {
    if (a > b) result
    else iter(next(a), result * term(a))
  }

  iter(a, 1)
}

def factorialIter(n: Double) = productIter((x: Double) => x, 1, (x: Double) => x + 1, n)

def piIter(n: Double) = 4 * productIter(piTerm, 1, (x: Double) => x + 1, n)

// test
println(s"factorial(5) = ${factorial(5)}")
println(s"1000th approx. of pi = ${pi(1000)}")
println("Iteratively:")
println(s"factorialIter(5) = ${factorialIter(5)}")
println(s"1000th approx. of pi = ${piIter(1000)}")
