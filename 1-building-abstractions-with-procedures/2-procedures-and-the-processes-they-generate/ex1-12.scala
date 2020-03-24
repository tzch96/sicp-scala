// Exercise 1.12:
// Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

def pascal(row: Int, col: Int): Int = if (col == 0) 1 else if (row == 0) col else (row * pascal(row - 1, col - 1)) / col

// test
println(pascal(0, 0))
println(pascal(2, 1))
println(pascal(3, 1))
println(pascal(4, 1))
