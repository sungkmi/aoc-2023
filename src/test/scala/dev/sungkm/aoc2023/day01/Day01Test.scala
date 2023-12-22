package dev.sungkm.aoc2023.day01

class Day01Test extends munit.FunSuite:

  val example = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

  test("calibrate"):
    assertEquals(Day01.calibrate("1abc2"), BigInt(12))
    assertEquals(Day01.calibrate("pqr3stu8vwx"), BigInt(38))
    assertEquals(Day01.calibrate("a1b2c3d4e5f"), BigInt(15))
    assertEquals(Day01.calibrate("treb7uchet"), BigInt(77))

  test("part1"):
    assertEquals(Day01.part1(example), BigInt(142))
  
  val example2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

  test("calibrate2"):
    assertEquals(Day01.calibrate2("two1nine"), BigInt(29))
    assertEquals(Day01.calibrate2("eightwothree"), BigInt(83))
    assertEquals(Day01.calibrate2("abcone2threexyz"), BigInt(13))
    assertEquals(Day01.calibrate2("xtwone3four"), BigInt(24))
    assertEquals(Day01.calibrate2("4nineeightseven2"), BigInt(42))
    assertEquals(Day01.calibrate2("zoneight234"), BigInt(14))
    assertEquals(Day01.calibrate2("7pqrstsixteen"), BigInt(76))

  test("part2"):
    assertEquals(Day01.part2(example2), BigInt(281))
  