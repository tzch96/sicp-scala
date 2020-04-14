// Exercise 1.19 (re-worded):
// Consider T: a <- a + b, b <- a to be a special case of p = 0 and q = 1 in a family of transformations T_pq,
// where T_pq transforms the pair (a, b) according to a <- bq + aq + ap and b <- bp + aq.
// If we apply such a transformation twice, the effect is the same as using a single transformation T_p'q' of the same form,
// where p' and q' can be computed in terms of p and q.
//
// Complete the given Fibonacci procedure, which runs in a logarithmic number of steps.

// Answer: p' = p*p + q*q
//         q' = 2*p*q + q*q

def fib(n: Int): Int = fibIter(1, 0, 0, 1, n)

def fibIter(a: Int, b: Int, p: Int, q: Int, count: Int): Int = {
  if (count == 0) b
  else if (count % 2 == 0)
    // implement the calculation for the third and fourth arguments here
    fibIter(a, b, p * p + q * q, 2 * p * q + q * q, count / 2)
  else fibIter(b * q + a * q + a * p, b * p + a * q, p, q, count - 1)
}

// test
for (i <- 0 to 20) print(s"${fib(i)} ")
println()
