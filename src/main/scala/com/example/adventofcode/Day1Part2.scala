package com.example.adventofcode

import com.example.adventofcode.Day1.Position

object Day1Part2 extends App {

  case class Position(x: Int, y: Int, facing: Char)

  val positions = Inputs.day1.split(", ")
    .toSeq
    .map(i => i.head -> i.tail.toInt)
    .flatMap { case (c, i) => (c, 1) +: Seq.fill(i - 1)(('N', 1)) }
    .scanLeft(Position(0, 0, 'N')) { (pos, tup) =>
      tup match {
        case ('L', n) if pos.facing == 'N' => Position(pos.x - n, pos.y, 'W')
        case ('L', n) if pos.facing == 'W' => Position(pos.x, pos.y - n, 'S')
        case ('L', n) if pos.facing == 'E' => Position(pos.x, pos.y + n, 'N')
        case ('L', n) if pos.facing == 'S' => Position(pos.x + n, pos.y, 'E')

        case ('R', n) if pos.facing == 'N' => Position(pos.x + n, pos.y, 'E')
        case ('R', n) if pos.facing == 'W' => Position(pos.x, pos.y + n, 'N')
        case ('R', n) if pos.facing == 'E' => Position(pos.x, pos.y - n, 'S')
        case ('R', n) if pos.facing == 'S' => Position(pos.x - n, pos.y, 'W')

        case ('N', n) if pos.facing == 'N' => Position(pos.x, pos.y + n, 'N')
        case ('N', n) if pos.facing == 'W' => Position(pos.x - n, pos.y, 'W')
        case ('N', n) if pos.facing == 'E' => Position(pos.x + n, pos.y, 'E')
        case ('N', n) if pos.facing == 'S' => Position(pos.x, pos.y - n, 'S')
      }
    }
    .map(position => position.x -> position.y  )

  println(positions)
  val dedup = positions.diff(positions.distinct)
  val dest = dedup.head
  println(Math.abs(dest._1) + Math.abs(dest._2))
}
