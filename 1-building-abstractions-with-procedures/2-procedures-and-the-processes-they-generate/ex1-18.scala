// Exercise 1.18:
// Devise a procedure that generates an iterative process for multiplying two integers in terms of
// adding, doubling, and halving and uses a logarithmic number of steps.

// Uses the "Russian peasant multiplication algorithm": double the first number and halve the second number repeatedly until
// the second number is 1. Whenever the second number becomes odd, add the first number to the result.

def multiply(a: Int, b: Int): Int = {
  def multiplyIter(result: Int, first: Int, second: Int): Int = {
    if (second == 0) result
    else if (second % 2 == 0) multiplyIter(result, first * 2, second / 2)
    else multiplyIter(result + first, first * 2, second / 2)
  }

  multiplyIter(0, a, b)
}

// test
println(multiply(2, 16))
println(multiply(50, 10))
println(multiply(1000, 1000))
