package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day09 extends ResourceApp.Simple:

  opaque type OasisReport = Seq[History]
  object OasisReport:
    def parse(input: String): OasisReport =
      input.split("\n").toSeq.map(History.parse)
  extension (or: OasisReport) def histories: Seq[History] = or

  opaque type History = Seq[BigInt]
  object History:
    def apply(vs: Seq[BigInt]): History = vs
    def parse(input: String): History =
      input.split(" ").toSeq.map(BigInt(_))
  extension (h: History)
    def diffs: History = History(h.tail.zip(h.init).map(_ - _))
    def next: BigInt =
      val ds = diffs
      if ds.toSet == Set(0) then h.last else h.last + ds.next
    def previous: BigInt =
      val ds = diffs
      if ds.toSet == Set(0) then h.head else h.head - ds.previous

  def part1(input: String): BigInt =
    OasisReport
      .parse(input)
      .histories
      .map(_.next)
      .sum

  def part2(input: String): BigInt =
    OasisReport
      .parse(input)
      .histories
      .map(_.previous)
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

  def run: Resource[IO, Unit] = printAns[IO]("input/day9")
