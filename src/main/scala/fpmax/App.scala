package fpmax

import scala.io.StdIn.readLine
import scala.util.Try

object App {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main(args: Array[String]): Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number from 1 to 5:")

      val guess = parseInt(readLine())
      guess match {
        case None => println("You did not enter the number")
        case Some(guess) =>
          if (guess == num) println("You guessed right, " + name + "!")
          else println("You guessed wrong, " + name + "! The number was: " + num)
      }

      var cont = true
      while (cont) {
        cont = false
        println("Do you want to continue, " + name + "?")

        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _ => cont = false
        }
      }
    }
  }
}

object Intro {
  /**
    * Functional programming is all about programming with functions.
    * Functions are:
    *
    * 1. Total. For every input, they return an output.
    * 2. Deterministic. For the same input, they return the same output.
    * 3. Pure. Their only effec is computing the return value.
    *
    * These properties help us:
    *
    * 1. Reason about our programs using equational reason.
    * 2. Refactor our program without changing their meaning.
    * 3. Test our programs more easily
    * 4. Invert control so caller always has control over callee
    * 5. Reason about our program using type-based reasoning
    *
    */

  def println(s: String): Unit = ???
  def readLine(): String = ???
  def parseInt(s: String): Int = ???
}