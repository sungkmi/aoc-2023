package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day03 extends ResourceApp.Simple:

  case class EngineSchematic(
    numberMap: Map[(Int, Int), Int],
    symbolPosition: Set[(Int, Int)],
  ):
    def partNumbers: Set[Int] = symbolPosition.flatMap:
      case (rowIndex, colIndex) =>
        val adjacentNumbers = for
          row <- rowIndex - 1 to rowIndex + 1
          col <- colIndex - 1 to colIndex + 1
          num <- numberMap.get((row, col)).toSeq
        yield num
        adjacentNumbers.toSet

  object EngineSchematic:
    def parse(input: String): EngineSchematic =
      val (numberMap, symbolPosition) = input.split("\n").zipWithIndex
        .foldLeft((Map.empty[(Int, Int), Int], Set.empty[(Int, Int)])):
          case ((numberMap, symbolPosition), (line, rowIndex)) =>
            val numberMap1 = "\\d+".r.findAllMatchIn(line)
              .flatMap: m =>
                (m.start to m.end).map: i =>
                  ((rowIndex, i), m.matched.toInt)
              .toMap
            val symbolPosition1 = for
              chWithIndex <- line.zipWithIndex
              (ch, colIndex) = chWithIndex
              if ch != '.' && !ch.isDigit
            yield (rowIndex, colIndex)
            (numberMap ++ numberMap1, symbolPosition ++ symbolPosition1.toSet)
      EngineSchematic(numberMap, symbolPosition)

  def part1(input: String): BigInt =
    EngineSchematic.parse(input).partNumbers.map(BigInt(_)).sum

  def printAns[F[_]: Console: Sync](path: String): Resource[F, Unit] =
    Resource
      .fromAutoCloseable:
        Sync[F].blocking:
          scala.io.Source.fromFile(path)
      .map(_.mkString)
      .evalMap: input =>
        for
          _ <- Console[F].println(part1(input))
//          _ <- Console[F].println(part2(input))
        yield ()

  def run: Resource[IO, Unit] = printAns[IO]("input/day3")
