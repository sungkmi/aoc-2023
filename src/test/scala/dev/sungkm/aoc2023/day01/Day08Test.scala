package dev.sungkm.aoc2023.day01

class Day08Test extends munit.FunSuite:

  val example = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

  test("part1"):
      assertEquals(Day08.part1(example), BigInt(2))

  val example2 = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ"""

  test("part1 example 2"):
      assertEquals(Day08.part1(example2), BigInt(6))

  val example3 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

  test("part2"):
      assertEquals(Day08.part2(example3), BigInt(6))

  test("gmap"):
    val map = Day08.NetworkMap.parse(example3)
    map.getGMap.foreach(println)
