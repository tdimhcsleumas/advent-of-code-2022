package com.github.tdimhcsleumas.calorie_counter

import zio._
import scala.io.Source

case class State(val cur: Int, val ret: Seq[Int])

object CalorieCounter extends ZIOAppDefault {
  def parseInt(s: String) =
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }

  def run = for {
    args <- getArgs

    filePath <-
      if (args.isEmpty)
        ZIO.fail("Please specify the input file!")
      else
        ZIO.succeed(args.head)
    _ <- Console.printLine(s"Reading file: ${filePath}!")

    lines <- ZIO.attempt(Source.fromFile(filePath).getLines)

    orderedSums <- ZIO.attempt(
      lines.map(parseInt)
        .foldLeft(State(0, List()))((state: State, next) => {
          next match {
            case Some(n) => State(state.cur + n, state.ret)
            case None => State(0, state.ret :+ state.cur)
          }
        })
        .ret
        .sorted(Ordering[Int].reverse)
    )

    _ <- Console.printLine(s"pt1: ${orderedSums(0)}")

    _ <- Console.printLine(s"pt2: ${orderedSums.slice(0, 3).sum}")

  } yield ()
}
