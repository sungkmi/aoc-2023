package dev.sungkm.aoc2023.day01

class Day07Test extends munit.FunSuite:

  val example = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

  test("part1"):
      assertEquals(Day07.part1(example), BigInt(6440))
