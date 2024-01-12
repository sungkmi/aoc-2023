package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day04 extends ResourceApp.Simple:

  final case class Card(
      winningNumbers: Set[Int],
      numbersYouHave: Seq[Int],
  ):
    val numberOfWinningNumbers: Int =
      numbersYouHave.filter(winningNumbers.contains).size

    def point: BigInt =
      if numberOfWinningNumbers <= 0 then BigInt(0)
      else BigInt(2).pow(numberOfWinningNumbers - 1)

  object Card:
    def parse(line: String): Card =
      val Array(front, back)      = line.split(" \\| ")
      val Array(_, numbersString) = front.split(": ")

      def getNumbers(s: String): Seq[Int] =
        s.sliding(2, 3).map(_.trim.toInt).toSeq

      val winningNumbers = getNumbers(numbersString).toSet
      val numbersYouHave = getNumbers(back)

      Card(winningNumbers, numbersYouHave)

  def getCardsFromInput(input: String): Seq[Card] =
    input.split("\n").map(Card.parse).toSeq

  def part1(input: String): BigInt =
    getCardsFromInput(input).map(_.point).sum

  def part2(input: String): BigInt =
    val cards = getCardsFromInput(input)
    cards
      .map(_.numberOfWinningNumbers)
      .zipWithIndex
      .foldLeft((0 until cards.size).map(_ -> BigInt(1)).toMap):
        case (count, (matchingNum, index)) =>
          (1 to matchingNum)
            .map(index + _)
            .foldLeft(count): (count, i) =>
              count.updated(i, count(i) + count(index))
      .values
      .sum

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

  def run: Resource[IO, Unit] = printAns[IO]("input/day4")
