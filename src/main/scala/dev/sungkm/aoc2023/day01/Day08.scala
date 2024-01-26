package dev.sungkm.aoc2023.day01

import cats.effect.{IO, Resource, ResourceApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.*

object Day08 extends ResourceApp.Simple:

  enum Instruction:
    case Left, Right
  object Instruction:
    def parse(ch: Char): Instruction = ch match
      case 'L' => Left
      case 'R' => Right
      case _ => throw new IllegalArgumentException(s"Invalid instruction: $ch")

  final case class NetworkMap(
      instructions: IndexedSeq[Instruction],
      network: Map[String, (String, String)],
  ):
    def countDest: BigInt =
      @annotation.tailrec
      def loop(current: String, dest: String, count: BigInt): BigInt =
        if current == dest then count
        else
          val (left, right) = network(current)
          instructions((count % instructions.length).toInt) match
            case Instruction.Left  => loop(left, dest, count + 1)
            case Instruction.Right => loop(right, dest, count + 1)
      loop("AAA", "ZZZ", 0)

    @annotation.tailrec
    def gmapLoop(
        current: String,
        instructions: List[Instruction],
        acc: List[Int],
        count: Int,
    ): (Seq[Int], String) =
//      println(s"Current: $current, acc: $acc, Count: $count")
      instructions match
        case Nil => (acc.reverse, current)
        case Instruction.Left :: tail =>
          val next = network(current)._1
          val acc1 = if current.endsWith("Z") then count :: acc else acc
          gmapLoop(next, tail, acc1, count + 1)
        case Instruction.Right :: tail =>
          val next = network(current)._2
          val acc1 = if current.endsWith("Z") then count :: acc else acc
          gmapLoop(next, tail, acc1, count + 1)

    def getGMap: Map[String, (Seq[Int], String)] =
      network.keys
        .map: start =>
          start -> gmapLoop(start, instructions.toList, Nil, 0)
        .toMap

    def countGhostPath: BigInt =
      val gMap = getGMap

      println("Ghost Map")
      gMap.foreach(println)
      println("------")

      @annotation.tailrec
      def loop(
          current: Seq[String],
          count: BigInt,
      ): BigInt =
        val currentGMaps = current.map(gMap)
        val destSet = currentGMaps.map(_._1.toSet)
        val commons = destSet.reduce(_ intersect _)
        if commons.nonEmpty then
          count + commons.min
        else
          val next = currentGMaps.map(_._2)
          loop(next, count + instructions.size)

      val starts = network.keys.filter(_ endsWith "A").toSeq
      loop(starts, 0)

  object NetworkMap:
    def parse(input: String): NetworkMap =
      val Array(instructionsString, networkString) = input.split("\n\n")
      val instructions = instructionsString.toCharArray.map(Instruction.parse)
      val network = networkString
        .split("\n")
        .map: line =>
          val Array(origin, dest) = line.split(" = ")
          val Array(left, right) =
            dest.stripPrefix("(").stripSuffix(")").split(", ")
          (origin, (left, right))
      NetworkMap(instructions.toIndexedSeq, network.toMap)

  def part1(input: String): BigInt = NetworkMap.parse(input).countDest

  def part2(input: String): BigInt =
    val map = NetworkMap.parse(input)
//    val result = map.gmapLoop("AAA", map.instructions.toList, Nil, 0)
//    println(s"Result: $result")


    val gMap = map.getGMap

    println("Ghost Map")
    gMap.foreach(println)
    println("------")

    def loop(current: String, count: BigInt): BigInt =
      println(s"#$count: $current")
      if current.endsWith("Z") then count else loop(gMap(current)._2, count + 1)

    loop("PXA", 0)
//    loop("FQJ", 0)


//    map.countGhostPath
// AAA -> 79, 1
// DVA -> 47, 1
// FQJ -> 59, 1
// JHA -> 67, 1
// NMA -> 61, 1
// PXA -> 53, 1
// 281


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

  def run: Resource[IO, Unit] = printAns[IO]("input/day8")
