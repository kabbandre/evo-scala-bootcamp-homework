package basics

import basics.ControlStructures.Command._

import scala.io.Source

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command {
    def calculate(): Double
  }
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def calculate(): Double = dividend / divisor
    }
    final case class Sum(numbers: List[Double]) extends Command {
      override def calculate(): Double = numbers.sum
    }
    final case class Average(numbers: List[Double]) extends Command {
      override def calculate(): Double = numbers.sum / numbers.length
    }
    final case class Min(numbers: List[Double]) extends Command {
      override def calculate(): Double = numbers.min
    }
    final case class Max(numbers: List[Double]) extends Command {
      override def calculate(): Double = numbers.max
    }
  }

  final case class ErrorMessage(value: String) {
    override def toString: String = s"Error: $value"
  }

  sealed trait Result
  final case class ResultMessage(command: Command) extends Result {
    override def toString: String = {
      command match {
        case Divide(dividend, divisor)    => s"$dividend divided by $divisor is ${command.calculate()}"
        case Sum(numbers)                 => s"the sum of ${numbers.mkString(" ")} is ${command.calculate()}"
        case Average(numbers)             => s"the average of ${numbers.mkString(" ")} is ${command.calculate()}"
        case Min(numbers)                 => s"the min of ${numbers.mkString(" ")} is ${command.calculate()}"
        case Max(numbers)                 => s"the max of ${numbers.mkString(" ")} is ${command.calculate()}"
      }
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    def parseNumbers(list: List[String]): List[Double] = {
      list.flatMap(num => num.toDoubleOption)
    }

    x.trim.split(" ").toList match {
      case x :: _   if x.isEmpty                  => Left(ErrorMessage("Empty line"))
      case _ :: Nil                               => Left(ErrorMessage("No arguments were passed"))
      case _ :: xs  if parseNumbers(xs).isEmpty   => Left(ErrorMessage("Invalid arguments, enter numbers"))
      case x :: xs  if x == "divide"              =>
        val numbers = parseNumbers(xs)
        if (numbers.length != 2) Left(ErrorMessage("Divide command only accepts 2 arguments"))
        else Right(Divide(numbers(0), numbers(1)))
      case x :: xs  if x == "sum"                 => Right(Sum(parseNumbers(xs)))
      case x :: xs  if x == "average"             => Right(Average(parseNumbers(xs)))
      case x :: xs  if x == "min"                 => Right(Min(parseNumbers(xs)))
      case x :: xs  if x == "max"                 => Right(Max(parseNumbers(xs)))
      case x :: _                                 => Left(ErrorMessage(s"Unknown command $x"))
    }
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor)  => divisor match {
        case 0 => Left(ErrorMessage("You cannot divide by zero"))
        case _ => Right(ResultMessage(Divide(dividend, divisor)))
      }
      case Sum(numbers)               => Right(ResultMessage(Sum(numbers)))
      case Average(numbers)           => Right(ResultMessage(Average(numbers)))
      case Min(numbers)               => Right(ResultMessage(Min(numbers)))
      case Max(numbers)               => Right(ResultMessage(Max(numbers)))
    }
  }


  def renderResult(x: Result): String = {
    x.toString
  }

  def process(x: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
    // ??? // implement using a for-comprehension
    for {
      results <- parseCommand(x) match {
        case Left(value) => value.toString
        case Right(value) => calculate(value) match {
          case Left(value) => value.toString
          case Right(value) => renderResult(value)
        }
      }
    } yield results
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

