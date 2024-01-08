package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day03 extends ResourceApp.Simple:

  case class EngineSchematic(
      numberIndex: IndexedSeq[Int],
      numberMap: Map[(Int, Int), Int],
      symbolPosition: Set[(Int, Int, Char)],
  ):
    def partNumbers: Seq[(Int, Int)] = symbolPosition
      .flatMap: (rowIndex, colIndex, _) =>
        for
          row   <- rowIndex - 1 to rowIndex + 1
          col   <- colIndex - 1 to colIndex + 1
          index <- numberMap.get((row, col)).toSeq
        yield index
      .toSeq
      .map: index =>
        (numberIndex(index), index)
    def gearRatios: Seq[BigInt] = for
      symbolPos <- symbolPosition.toSeq
      (rowIndex, colIndex, symbol) = symbolPos if symbol == '*'
      gearCandidates =
        val seq = for
          col   <- colIndex - 1 to colIndex + 1
          row   <- rowIndex - 1 to rowIndex + 1
          index <- numberMap.get((row, col)).toSeq
        yield index
        seq.toSet
      if gearCandidates.toSet.size == 2
    yield gearCandidates.map(numberIndex(_)).map(BigInt(_)).product

  object EngineSchematic:
    def parse(input: String): EngineSchematic =
      val linesWithIndex = input.split("\n").zipWithIndex
      val numbers = linesWithIndex.foldLeft(List.empty[(Int, (Int, Int, Int))]):
        case (numbers, (line, rowIndex)) =>
          val matched = "\\d+".r
            .findAllMatchIn(line)
            .map: m =>
              (m.matched.toInt, (rowIndex, m.start, m.end))
          matched.toList.reverse ::: numbers
      val symbolPosition = linesWithIndex.foldLeft(Set.empty[(Int, Int, Char)]):
        case (symbolPosition, (line, rowIndex)) =>
          val symbolPosition1 = for
            chWithIndex <- line.zipWithIndex
            (ch, colIndex) = chWithIndex
            if ch != '.' && !ch.isDigit
          yield (rowIndex, colIndex, ch)
          symbolPosition ++ symbolPosition1.toSet
      val (numberIndex, numberPositions) = numbers.reverse.unzip
      val numberMap =
        numberPositions.zipWithIndex.foldLeft(Map.empty[(Int, Int), Int]):
          case (numberMap, ((rowIndex, colStart, colEnd), numberIndex)) =>
            numberMap ++ (colStart until colEnd).map: colIndex =>
              ((rowIndex, colIndex), numberIndex)
      EngineSchematic(numberIndex.toIndexedSeq, numberMap, symbolPosition)

  def part1(input: String): BigInt =
    val engineSchematic = EngineSchematic.parse(input)
    engineSchematic.partNumbers.map(_._1).map(BigInt(_)).sum

  def part2(input: String): BigInt =
    val engineSchematic = EngineSchematic.parse(input)
    engineSchematic.gearRatios.sum

  def printAns[F[_]: Console: Sync](path: String): Resource[F, Unit] =
    Resource
      .fromAutoCloseable:
        Sync[F].blocking:
          scala.io.Source.fromFile(path)
      .map(_.mkString)
      .evalMap: input =>
        for
          _ <- Console[F].println(part1(input))
          _ <- Console[F].println(part2(input))
        yield ()

  def run: Resource[IO, Unit] = printAns[IO]("input/day3")
