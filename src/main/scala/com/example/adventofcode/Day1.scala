package com.example.adventofcode

object Day1 extends App {

  case class Position(x: Int, y: Int, facing: Char)

  // Part 1
  val dest = Inputs.day1.split(", ")
    .map(i => i.head -> i.tail.toInt)
        .foldLeft(Position(0, 0, 'N')) { (pos, tup) =>
          tup match {
            case ('L', n) if pos.facing == 'N' => Position(pos.x - n, pos.y, 'W')
            case ('L', n) if pos.facing == 'W' => Position(pos.x, pos.y - n, 'S')
            case ('L', n) if pos.facing == 'E' => Position(pos.x, pos.y + n, 'N')
            case ('L', n) if pos.facing == 'S' => Position(pos.x + n, pos.y, 'E')

            case ('R', n) if pos.facing == 'N' => Position(pos.x + n, pos.y, 'E')
            case ('R', n) if pos.facing == 'W' => Position(pos.x, pos.y + n, 'N')
            case ('R', n) if pos.facing == 'E' => Position(pos.x, pos.y - n, 'S')
            case ('R', n) if pos.facing == 'S' => Position(pos.x - n, pos.y, 'W')
          }
        }

  println(dest)
  println(Math.abs(dest.x) + Math.abs(dest.y))

}
