package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day01 extends ResourceApp.Simple:

  def calibrate(line: String): BigInt =
    val digits = line.filter(_.isDigit).toVector
    val first = digits.head.toString
    val second = digits.last.toString
    BigInt(first ++ second)

  def calibrate2(line: String): BigInt =
    val digitWords =
      (1 to 9) zip "one, two, three, four, five, six, seven, eight, nine".split(", ")

    def filterMinusOne(n: Int): Option[Int] = if n == -1 then None else Some(n)

    val wordIndexes = digitWords.map: (digit, word) =>
      val first = line.indexOfSlice(word)
      val last = line.lastIndexOfSlice(word)
      (digit, filterMinusOne(first), filterMinusOne(last))
    
    val minDigitIndexOption = filterMinusOne(line.indexWhere(_.isDigit))
    val maxDigitIndexOption = filterMinusOne(line.lastIndexWhere(_.isDigit))

    val firstOption = wordIndexes
      .flatMap:
        case (digit, first, _) => first.map((digit, _))
      .minByOption(_._2)

    val lastOption = wordIndexes
      .flatMap:
        case (digit, _, last) => last.map((digit, _))
      .maxByOption(_._2)

    val first = (firstOption, minDigitIndexOption) match
      case (Some((digit, first)), Some(minDigitIndex)) =>
        if first < minDigitIndex then digit
        else line(minDigitIndex).toString.toInt
      case (Some((digit, first)), None) => digit
      case (None, Some(minDigitIndex)) => line(minDigitIndex).toString.toInt
      case (None, None) => -1
  
    val second = (lastOption, maxDigitIndexOption) match
      case (Some((digit, last)), Some(maxDigitIndex)) =>
        if last > maxDigitIndex then digit
        else line(maxDigitIndex).toString.toInt
      case (Some((digit, last)), None) => digit
      case (None, Some(maxDigitIndex)) => line(maxDigitIndex).toString.toInt
      case (None, None) => -1
  
    BigInt(first.toString ++ second.toString)


  def part1(input: String): BigInt = input.split("\n").map(calibrate).sum
  def part2(input: String): BigInt = input.split("\n").map(calibrate2).sum

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

  def run: Resource[IO, Unit] = printAns[IO]("input/day1")
