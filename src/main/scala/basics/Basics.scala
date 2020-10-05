// Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

// def lcm(a: Int, b: Int): Int = ???
// def gcd(a: Int, b: Int): Int = ???

package basics

class Basics extends App {
  def lcm(a: Int, b: Int): Int = if (List(a, b).contains(0)) 0 else (a * b).abs / gcd(a, b)
  def gcd(a: Int, b: Int): Int = {
    val aAbsolute = a.abs
    val bAbsolute = b.abs
    if (Math.min(aAbsolute, bAbsolute) == 0) Math.max(aAbsolute, bAbsolute) else gcd(bAbsolute, aAbsolute % bAbsolute)
  }
  // lcm or gcd = 0 means that lcm or gcd does not exist for such pair of numbers
}
