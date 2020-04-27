// Exercise 1.29:
// Using Simpson's Rule, the integral of a function f between a and b is approximated as:
//   h/3 * (y[0] + 4*y[1] + 2*y[2] + 4*y[3] + 2*y[4] + ... + 2*y[n-2] + 4*y[n-1] + y[n]),
// where h = (b - a)/n, for some even integer n, and y[k] = f(a + k*h).
// (Increasing n increases the accuracy of the approximation.)
//
// Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral,
// computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000),
// and compare the results to those of the integral procedure shown above.

def cube(x: Double) = x * x * x

def inc(n: Double) = n + 1

def sum(term: Double => Double, a: Double, next: Double => Double, b: Double): Double =
  if (a > b) 0
  else term(a) + sum(term, next(a), next, b)

def integral(f: Double => Double, a: Double, b: Double, dx: Double) = {
  def addDx(x: Double) = x + dx

  sum(f, a + dx / 2.0, addDx, b) * dx
}

def simpson(f: Double => Double, a: Double, b: Double, n: Double) = {
  def h = (b - a) / n

  def y(k: Double) = f(a + k * h)

  def term(k: Double) = {
    def multiplier =
      if (k == 0 || k == n) 1.0
      else if (k % 2 == 1) 4.0
      else 2.0

    multiplier * y(k)
  }

  (h / 3) * sum(term, 0.0, inc, n)
}

// test
println("Using integral procedure (dx = 0.01 and dx = 0.001):")
println(integral(cube, 0, 1, 0.01))
println(integral(cube, 0, 1, 0.001))

println("Using Simpson's rule (n = 100 and n = 1000):")
println(simpson(cube, 0, 1, 100))
println(simpson(cube, 0, 1, 1000))
