package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day05 extends ResourceApp.Simple:

  case class Almanac(
      seeds: Seq[BigInt],
      maps: Seq[AlmanacMap],
  ):
    def locations: Seq[BigInt] = seeds.map: seed =>
      maps.foldLeft(seed): (source, map) =>
        map.convert(source)

    def rangeLocations: Seq[(BigInt, BigInt)] =
      val seedRanges = seeds
        .sliding(2, 2)
        .toSeq
        .map:
          case Seq(start, range) => (start, start + range)

      println(s"Seed Ranges: ${seedRanges.sorted}")
      maps.foldLeft(seedRanges): (sourceRanges, map) =>
        val result = map.convertRanges(sourceRanges)
        println(s"map: ${map}")
        println(s"===> result: ${result.sorted}")
        result

  object Almanac:
    def parse(input: String): Almanac =
      val lines = input.split("\n\n").toSeq
      val seeds =
        lines.head.stripPrefix("seeds: ").split(" ").map(BigInt(_)).toSeq
      val maps = lines.tail.map(AlmanacMap.parse)

      Almanac(seeds, maps)

  case class AlmanacMap(mappingRanges: Seq[MappingRanges]):
    def convert(sourceIndex: BigInt): BigInt =
      def loop(mappingRanges: List[MappingRanges]): BigInt =
        mappingRanges match
          case Nil => sourceIndex
          case mappingRange :: tail =>
            mappingRange.convert(sourceIndex) match
              case Some(destIndex) => destIndex
              case None            => loop(tail)
      loop(mappingRanges.toList)
    def convertRanges(ranges: Seq[(BigInt, BigInt)]): Seq[(BigInt, BigInt)] =
      val (convertedRanges, unconvertedRanges) =
        mappingRanges.foldLeft((Seq.empty[(BigInt, BigInt)], ranges)):
          case ((convertedRanges, unconvertedRanges), mappingRange) =>
            val (convertedRangeOptions, unconvertedRanges1) =
              unconvertedRanges.map(mappingRange.convertRange).unzip
            (
              convertedRanges ++ convertedRangeOptions.map(_.toSeq).flatten,
              unconvertedRanges1.flatten,
            )
      convertedRanges ++ unconvertedRanges

  object AlmanacMap:
    def parse(input: String): AlmanacMap =
      val mappingRanges = input
        .split("\n")
        .tail
        .toSeq
        .map: line =>
          val Array(dest, source, length) = line.split(" ").map(BigInt(_))
          MappingRanges(dest, source, length)
      AlmanacMap(mappingRanges)

  case class MappingRanges(dest: BigInt, source: BigInt, length: BigInt):
    def convert(sourceIndex: BigInt): Option[BigInt] =
      if sourceIndex >= source && sourceIndex < source + length then
        Some(dest + sourceIndex - source)
      else None
    def convertRange(
        range: (BigInt, BigInt),
    ): (Option[(BigInt, BigInt)], Seq[(BigInt, BigInt)]) =
      val (start, end) = range
      if end <= source || start >= source + length then (None, Seq(range))
      else if start >= source && end <= source + length then
        (Some((dest + start - source, dest + end - source)), Seq.empty)
      else if start >= source then
        /*
              | -------- | ----------- | -------------- |
             source      start   source + length       end
          ==> (dest + start - source, dest + length), (source + length, end)
         */
        (
          Option((dest + start - source, dest + length)),
          Seq((source + length, end)),
        )
      else if end <= source + length then
        /*
              | -------- | ----------- | -------------- |
            start      source         end       source + length
          ==> (dest, dest + end - source), (start, source)
         */

        (Option((dest, dest + end - source)), Seq((start, source)))
      else
        /*
              | -------- | ----------- | -------------- |
            start      source         source + length       end
          ==> (dest, dest + length), (start, source), (source + length, end)
         */
        (
          Option((dest, dest + length)),
          Seq((start, source), (source + length, end)),
        )

  def part1(input: String): BigInt = Almanac.parse(input).locations.min

  def part2(input: String): BigInt = Almanac.parse(input).rangeLocations.min._1

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

  def run: Resource[IO, Unit] = printAns[IO]("input/day5")
