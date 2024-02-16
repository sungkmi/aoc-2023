package dev.sungkm.aoc2023.day01

class Day09Test extends munit.FunSuite:

//  test("history"):
//    assertEquals(Day09.History.parse("3 3 3 3").next, 3)

//  test("part1-1"):
//    assertEquals(Day09.part1("0 3 6 9 12 15"), BigInt(18))

  val example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

  test("part1"):
      assertEquals(Day09.part1(example), BigInt(114))

  test("part2"):
      assertEquals(Day09.part2(example), BigInt(2))
