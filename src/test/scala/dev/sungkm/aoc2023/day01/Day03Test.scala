package dev.sungkm.aoc2023.day01

class Day03Test extends munit.FunSuite:

  val example = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

  test("part1"):
      assertEquals(Day03.part1(example), BigInt(4361))

  test("part2"):
      assertEquals(Day03.part2(example), BigInt(467835))
