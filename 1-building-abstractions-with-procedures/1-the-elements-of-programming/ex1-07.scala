// Exercise 1.7:
// (Re-worded) Change the good-enough test from the square root example to work properly for very small and very large numbers.
//
// Original good-enough test (in Lisp):
//  (define (good-enough? guess x)
//    (< (abs (- (square guess) x)) 0.001))

def squareRoot(x: Double): Double = {
  def isGoodEnough(guess: Double, previousGuess: Double): Boolean = {
    math.abs(guess - previousGuess) < (guess * 0.001)
  }

  def improve(guess: Double): Double = {
    (guess + (x / guess)) / 2
  }

  def sqrtIter(guess: Double, previousGuess: Double): Double = {
    if (isGoodEnough(guess, previousGuess))
      guess
    else
      sqrtIter(improve(guess), guess)
  }

  sqrtIter(1.0, 2.0)
}

// test
println(s"sqrt(9): ${squareRoot(9)}")
println(s"sqrt(1e-18): ${squareRoot(1e-18)}")
println(s"sqrt(1e18): ${squareRoot(1e18)}")
