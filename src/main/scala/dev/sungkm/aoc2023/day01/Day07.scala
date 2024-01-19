package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day07 extends ResourceApp.Simple:

  opaque type Card = Char
  object Card:
    def apply(c: Char): Card = c
  extension (c: Card)
    def order: Int =
      inline def map = "AKQJT98765432".zipWithIndex.toMap
      map(c)
  end extension
  given Ordering[Card] with
    def compare(x: Card, y: Card): Int =
      println(s"comparing card $x and $y")
      x.order - y.order
    
  opaque type Hand = Seq[Card]
  object Hand:
    def apply(cards: Seq[Card]): Hand = cards

  extension (h: Hand)
    def cards: Seq[Card] = h
    def `type`: Int =
      cards.groupMapReduce(_.order)(_ => 1)(_ + _).toSeq.map(_._2).sortBy(-_).take(2).toList match
        case 5 :: xs => 0
        case 4 :: xs => 1
        case 3 :: 2 :: xs => 2
        case 3 :: xs => 3
        case 2 :: xs => 4
        case _ => 5
    def compareTo(other: Hand): Int =
      println(s"===> comparing $h and $other")
      if (`type` > other.`type`) 1
      else if (`type` < other.`type`) -1
      else
        cards.map(_.order).compare(other.cards.map(_.order))

  given Ordering[Hand] with
    def compare(x: Hand, y: Hand): Int =
      println(s"comparing $x and $y")
      x compareTo y

  def parse(input: String): Seq[(Hand, BigInt)] =
    input.split("\n").toSeq.map: line =>
      val Array(handString, bidString) = line.split(" ")
      Hand(handString.map(Card(_))) -> BigInt(bidString)

  def part1(input: String): BigInt =
    val parsed = parse(input)
    println(s"parsed: $parsed")
    val sorted = parsed.sortBy(_._1).reverse
    println(s"sorted: $sorted")
    sorted.zipWithIndex
      .map:
        case ((hand, bid), index) => bid * (index + 1)
      .sum

  def part2(input: String): BigInt = ???

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

  def run: Resource[IO, Unit] = printAns[IO]("input/day7")
