package com.github.tdimhcsleumas.calorie_counter

import zio._
import scala.io.Source
import scala.util.matching.Regex

sealed trait Shape
case class Rock() extends Shape
case class Paper() extends Shape
case class Scissors() extends Shape

sealed trait Outcome
case class Win() extends Outcome
case class Loss() extends Outcome
case class Draw() extends Outcome

object DecodeRps extends ZIOAppDefault {
    def mapPt1(l: String): Int = {
        def mapChars(n: String): Option[Shape] = {
            n match {
                case "A" | "X" => Some(Rock())
                case "B" | "Y" => Some(Paper())
                case "C" | "Z" => Some(Scissors())
                case _ => None
            }
        }

        val rowRegex: Regex = """^(\w) (\w)$""".r

        val (them, us) = (l match {
            case rowRegex(them, us) => (mapChars(them), mapChars(us)) 
            case _ => (None, None)
        }) match {
            case (Some(them), Some(us)) => (them, us)
            case _ => return 0
        }
        
        val shapePoint = us match {
            case Rock() => 1
            case Paper() => 2
            case Scissors() => 3
        }

        val winPoints = (us, them) match {
            case (Paper(), Rock()) | (Rock(), Scissors()) | (Scissors(), Paper()) => 6
            case _ =>
                if (us == them) 3
                else 0
        }

        shapePoint + winPoints
    }

    def mapPt2(l: String): Int = {
        def mapChars(n: String): Option[Shape] = {
            n match {
                case "A" => Some(Rock())
                case "B" => Some(Paper())
                case "C" => Some(Scissors())
                case _ => None
            }
        }

        def mapGoal(n: String): Option[Outcome] = {
            n match {
                case "X" => Some(Loss())
                case "Y" => Some(Draw())
                case "Z" => Some(Win())
                case _ => None
            }
        }

        val rowRegex: Regex = """^(\w) (\w)$""".r

        val (them, outcome) = (l match {
            case rowRegex(them, outcome) => (mapChars(them), mapGoal(outcome)) 
            case _ => (None, None)
        }) match {
            case (Some(them), Some(outcome)) => (them, outcome)
            case _ => return 0
        }

        val winPoints = outcome match {
            case Win() => 6
            case Draw() => 3 
            case Loss() => 0
        }

        val targetShape = outcome match {
            case Win() => them match {
                case Rock() => Paper()
                case Paper() => Scissors()
                case Scissors() => Rock()
            }
            case Loss() => them match {
                case Rock() => Scissors()
                case Paper() => Rock()
                case Scissors() => Paper()
            }
            case _ => them
        }

        val shapePoint = targetShape match {
            case Rock() => 1
            case Paper() => 2
            case Scissors() => 3
        }

        shapePoint + winPoints
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

        sumPt1 <- ZIO.succeed(
            lines.map(mapPt1)
                .sum
        )

        _ <- Console.printLine(s"result1: ${sumPt1}")

        sumPt2 <- ZIO.succeed(
            Source.fromFile(filePath).getLines.map(mapPt2)
                .sum
        )

        _ <- Console.printLine(s"result2: ${sumPt2}")

  } yield ()
}
