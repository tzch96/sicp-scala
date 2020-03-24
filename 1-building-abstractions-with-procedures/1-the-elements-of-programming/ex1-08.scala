// Exercise 1.8:
// Implement a cube-root procedure analogous to the square-root procedure based on Newton's method.

def cubeRoot(x: Double): Double = {
  def improve(guess: Double): Double =
    ((x / (guess * guess)) + guess + guess) / 3

  def isGoodEnough(guess: Double, previousGuess: Double): Boolean =
    math.abs(guess - previousGuess) < math.abs(guess * 0.001)

  def cubeRootIter(guess: Double, previousGuess: Double): Double = {
    if (isGoodEnough(guess, previousGuess))
      guess
    else
      cubeRootIter(improve(guess), guess)
  }

  cubeRootIter(1.0, 0.0)
}

// test
println(cubeRoot(1))
println(cubeRoot(-8))
println(cubeRoot(1e-60))
println(cubeRoot(1e60))
