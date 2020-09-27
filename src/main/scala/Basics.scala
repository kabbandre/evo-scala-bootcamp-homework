// Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

// def lcm(a: Int, b: Int): Int = ???
// def gcd(a: Int, b: Int): Int = ???

package Basics

class Basics extends App {
  def lcm(a: Int, b: Int): Int = if (List(a, b).exists(_ == 0)) 0 else (a * b).abs / gcd(a, b)
  def gcd(a: Int, b: Int): Int = {
    val a_absolute = a.abs
    val b_absolute = b.abs
    if (Math.min(a_absolute, b_absolute) == 0) Math.max(a_absolute, b_absolute) else gcd(b_absolute, a_absolute % b_absolute)
  }
  // lcm or gcd = 0 means that lcm or gcd does not exist for such pair of numbers
}
