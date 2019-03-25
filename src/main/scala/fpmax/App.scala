package fpmax

import scala.io.StdIn.readLine
import scala.util.Try

object App {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  final case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }
  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  def putStrLn(line: String): IO[Unit] = IO(() => println(line))
  def getStrLn: IO[String] = IO(() => readLine())

  def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))

  def checkContinue(name: String): IO[Boolean] =
    for {
      _     <- putStrLn("Do you want to continue, " + name + "?")
      input <- getStrLn.map(_.toLowerCase)
      cont  <- input match {
                  case "y" => IO.point(true)
                  case "n" => IO.point(false)
                  case _ => checkContinue(name)
                }
    } yield cont

  def gameLoop(name: String): IO[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      n     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      _     <- parseInt(input).fold(
                 putStrLn("You did not enter the number")
               )(guess =>
                if (guess == num) putStrLn("You guessed right, " + name + "!")
                else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
               )
      cont  <- checkContinue(name)
      _     <- if(cont) gameLoop(name) else IO.point(())
    } yield ()

  def main(args: Array[String]): Unit = {
    for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn("Hello, " + name + ", welcome to the game!")
      _    <- gameLoop(name)
    } yield ()

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