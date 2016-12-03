package com.example.adventofcode

import com.example.adventofcode.Day1.Position

object Day2 extends App {

  case class Position(x: Int, y: Int, facing: Char)
  case class Accumulation(current: Position, history: Seq[Position])

  // Part 1
  val positions = Inputs.day1.split(", ")
    .map(i => i.head -> i.tail.toInt)
    .scanLeft(Position(0, 0, 'N')) { (pos, tup) =>
      tup match {
        case ('L', n) if pos.facing == 'N' =>
          Position(pos.x - n, pos.y, 'W')
        case ('L', n) if pos.facing == 'W' => Position(pos.x, pos.y - n, 'S')
        case ('L', n) if pos.facing == 'E' => Position(pos.x, pos.y + n, 'N')
        case ('L', n) if pos.facing == 'S' => Position(pos.x + n, pos.y, 'E')

        case ('R', n) if pos.facing == 'N' => Position(pos.x + n, pos.y, 'E')
        case ('R', n) if pos.facing == 'W' => Position(pos.x, pos.y + n, 'N')
        case ('R', n) if pos.facing == 'E' => Position(pos.x, pos.y - n, 'S')
        case ('R', n) if pos.facing == 'S' => Position(pos.x - n, pos.y, 'W')
      }
    }
  //    .toList
  //    .map(pos => pos.x -> pos.y)

  val dest = positions.diff(positions.distinct).head
  //  val dest = positions.filter(revisited.contains)

  println(positions)
  println(dest)
  //  println(Math.abs(dest.x) + Math.abs(dest.y))



}
