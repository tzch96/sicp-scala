// Exercise 1.32:
// Show that sum and product (Exercise 1.31) are both special cases of a still more general notion
// called accumulate that combines a collection of terms, using some general accumulation function:
//   (accumulate combiner null-value term a next b)
// 
// accumulate takes as arguments the same term and range specifications as sum and product,
// together with a combiner procedure (of two arguments) that specifies how the current term is to be
// combined with the accumulation of the preceding terms and a null-value that specifies what base
// value to use when the terms run out.
// 
// Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
//
// If your accumulate procedure generates a recursive process, write one that generates an iterative
// process. If it generates an iterative process, write one that generates a recursive process.

def accumulate(combiner: (Double, Double) => Double, nullValue: Double, term: Double => Double, a: Double, next: Double => Double, b: Double): Double = {
  if (a > b) nullValue
  else combiner(term(a), accumulate(combiner, nullValue, term, next(a), next, b))
}

def sum(term: Double => Double, a: Double, next: Double => Double, b: Double) = accumulate((x: Double, y: Double) => x + y, 0, term, a, next, b)

def product(term: Double => Double, a: Double, next: Double => Double, b: Double) = accumulate((x: Double, y: Double) => x * y, 1, term, a, next, b)

// test
println(sum((x: Double) => x, 1, (x: Double) => x + 1, 3))
