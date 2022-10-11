import scala.annotation.unused

@unused
def sum1(f: Int => Int = x => x): (Int, Int) => Int =
  def sumF(a: Int, b: Int): Int =
    if a > b then 0
    else f(a) + sumF(a + 1, b)
  sumF

// or equivalently:

def sum(f: Int => Int = x => x)(a: Int, b: Int): Int =
  if a > b then 0 else f(a) + sum(f)(a + 1, b)

sum()(1, 10)

/* Exercise
1. Write a product function that calculates the product of the values of a function for the integer points on a given interval
2. Write factorial in terms of product
3. Can you write a more general function, which generalizes both sum and product?
*/

def product(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1 else f(a) * product(f)(a + 1, b)

def fact(n: Int) = product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) =
  def recur(a: Int): Int =
    if a > b then zero
    else combine(f(a), recur(a + 1))
  recur(a)

def summation(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)
def multiplication(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)

summation(x => x)(1, 5)
summation(fact)(1, 5)

multiplication(x => x)(1, 5)