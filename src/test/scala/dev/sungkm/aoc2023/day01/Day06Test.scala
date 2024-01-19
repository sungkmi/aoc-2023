package dev.sungkm.aoc2023.day01

class Day06Test extends munit.FunSuite:

  val example = """Time:      7  15   30
Distance:  9  40  200"""

  test("part1"):
      assertEquals(Day06.part1(example), BigInt(288))

  test("part2"):
      assertEquals(Day06.part2(example), BigInt(71503))
