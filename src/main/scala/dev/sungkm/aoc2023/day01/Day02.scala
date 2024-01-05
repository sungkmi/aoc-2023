package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day02 extends ResourceApp.Simple:

  case class Game(id: Int, sets: Seq[CubeSet])
  opaque type CubeSet = Map[String, Int]
  object CubeSet:
    def apply(red: Int, green: Int, blue: Int): CubeSet =
      Map("red" -> red, "green" -> green, "blue" -> blue)
    def apply(cubeCountMap: Map[String, Int]): CubeSet = cubeCountMap
  extension (cubeSet: CubeSet)
    def red: Int   = cubeSet.getOrElse("red", 0)
    def green: Int = cubeSet.getOrElse("green", 0)
    def blue: Int  = cubeSet.getOrElse("blue", 0)

    def toCubeCountMap: Map[String, Int] = cubeSet
  end extension

  val limit = CubeSet(red = 12, green = 13, blue = 14)

  object Game:
    def parse(line: String): Game = 
      val Array(idString, setsString) = line.stripPrefix("Game ").split(": ")
      val sets = setsString
        .split("; ")
        .toSeq
        .map: setString =>
          setString
            .split(", ")
            .map: cubeString =>
              val Array(countString, color) = cubeString.split(" ")
              Map(color -> countString.toInt)
            .foldLeft(Map.empty[String, Int])(_ ++ _)
        .map(CubeSet(_))
      Game(idString.toInt, sets)

    extension (cubeSet: CubeSet)
      def isBelowLimit: Boolean = cubeSet.red <= limit.red &&
        cubeSet.green <= limit.green &&
        cubeSet.blue <= limit.blue
    end extension

    extension (game: Game)
      def isBelowLimit: Boolean =
        game.sets.forall(_.isBelowLimit)
      def maxNumberOfColors: CubeSet =
        CubeSet(game.sets.flatMap(_.toCubeCountMap).groupMapReduce(_._1)(_._2)(_ max _))
    end extension

  def part1(input: String): BigInt =
    input.split("\n").map(Game.parse).filter(_.isBelowLimit).map(_.id).sum
  def part2(input: String): BigInt = input
    .split("\n")
    .map(Game.parse)
    .map: game =>
      game.maxNumberOfColors.toCubeCountMap.values.product
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

  def run: Resource[IO, Unit] = printAns[IO]("input/day2")
