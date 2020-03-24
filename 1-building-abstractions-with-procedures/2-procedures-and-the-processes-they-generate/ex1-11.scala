// Exercise 1.11:
// A function f is defined by the rule that
// 
// f(n) = { n if n < 3,
//        { f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3.
// 
// Write a procedure that computes f recursively, and a procedure that computes f iteratively.

def fRec(n: Double): Double = {
  if (n < 3)
    n
  else
    fRec(n - 1) + 2 * fRec(n - 2) + 3 * fRec(n - 3)
}

def fI(n: Double): Double = {
  def fIter(a: Double, b: Double, c: Double, count: Double): Double = {
    if (count <= 0) a
    else fIter(b, c, c + (2 * b) + (3 * a), count - 1)
  }

  fIter(0, 1, 2, n)
}

// test
for (i <- 1 to 10) print(s"Recursively: ${fRec(i)}, iteratively: ${fI(i)}\n")
for (i <- List(2.5, 4.5, 5.2)) print(s"Recursively: ${fRec(i)}, iteratively: ${fI(i)}\n")
