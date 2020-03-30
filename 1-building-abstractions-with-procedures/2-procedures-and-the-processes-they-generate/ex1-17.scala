// Exercise 1.17:
// Design a multipication procedure that uses a logarithmic number of steps, using doubling and halving operations (on integers).

def double(n: Int): Int = n * 2
def halve(n: Int): Int = n / 2

def multiply(a: Int, b: Int): Int = {
  if (b == 0) 0
  else if (b % 2 == 0) double(a * halve(b))
  else a + (a * (b - 1))
}

// test
println(multiply(4, 8))
println(multiply(12, 12))
println(multiply(100, 100))
