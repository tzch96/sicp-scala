// Exercise 1.3:
// Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

def sumOfSquares(x: Double, y: Double): Double = (x * x) + (y * y)

def exercise(x: Double, y: Double, z: Double): Double = {
  val smallest = math.min(x, math.min(y, z))

  if (x == smallest) sumOfSquares(y, z)
  else if (y == smallest) sumOfSquares(x, z)
  else sumOfSquares(x, y)
}

// test
println(s"1, 2, 3: ${exercise(1, 2, 3)}")
println(s"10, 7, 5: ${exercise(10, 7, 5)}")
