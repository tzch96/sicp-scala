// Exercise 1.30:
// The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum
// is performed iteratively. Show how to do this by filling in the missing expressions in the following
// definition:
// (define (sum term a next b)
//   (define (iter a result)
//     (if <??>
//        <??>
//        (iter <??> <??>)))
//   (iter <??> <??>))

def term(x: Double) = x

def next(x: Double) = x + 1

def sum(term: Double => Double, a: Double, next: Double => Double, b: Double) = {
  def iter(a: Double, result: Double): Double = {
    if (a > b) result
    else iter(next(a), result + term(a))
  }

  iter(a, 0.0)
}

// test
println(sum(term, 1.0, next, 3.0))
