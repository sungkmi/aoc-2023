package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day06 extends ResourceApp.Simple:

  def numberOfWaysToBeatTheRecord(time: BigInt, dist: BigInt): BigInt =
    val count = (BigInt(1) to time).count: buttonHoldingTime =>
      val currentDist = (time - buttonHoldingTime) * buttonHoldingTime
      val result      = currentDist > dist
      result
    BigInt(count)

  case class Races(times: Seq[BigInt], distances: Seq[BigInt]):
    def numberOfWays: Seq[BigInt] = (times zip distances).map:
      (time, dist) => numberOfWaysToBeatTheRecord(time, dist)

  object Races:
    def parse(input: String): Races =
      val Array(timeLine, distLine) = input.split("\n")
      val numberPattern             = "\\d+".r
      val times     = numberPattern.findAllIn(timeLine).map(BigInt(_)).toSeq
      val distances = numberPattern.findAllIn(distLine).map(BigInt(_)).toSeq
      Races(times, distances)

  case class KerningConsideredRace(time: BigInt, dist: BigInt):
    def numberOfWays: BigInt = numberOfWaysToBeatTheRecord(time, dist)

  object KerningConsideredRace:
    def parse(input: String): KerningConsideredRace =
      val Array(time, dist) = input
        .split("\n")
        .map: line =>
          val Array(name, value) = line.split(": ")
          BigInt(value.replaceAll(" ", ""))
      KerningConsideredRace(time, dist)

  def part1(input: String): BigInt =
    val ways = Races.parse(input).numberOfWays
    ways.product

  def part2(input: String): BigInt =
    KerningConsideredRace.parse(input).numberOfWays

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

  def run: Resource[IO, Unit] = printAns[IO]("input/day6")
