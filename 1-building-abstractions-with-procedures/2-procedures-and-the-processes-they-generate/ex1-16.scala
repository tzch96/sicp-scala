// Exercise 1.16:
// Design a procedure that evolves an iterative exponentation process that uses successive squaring
// and uses a logarithmic number of steps.

def pow(a: Int, n: Int): Long = {
  def powIter(m: Int, y: Int, x: Int): Long = {
    if (m == 0) x
    else if (m % 2 == 0) powIter(m / 2, y * y, x)
    else powIter(m - 1, y, y * x)
  }

  powIter(n, a, 1)
}

// test
println(pow(2, 3))
println(pow(2, 16))
println(pow(1, 1000))
