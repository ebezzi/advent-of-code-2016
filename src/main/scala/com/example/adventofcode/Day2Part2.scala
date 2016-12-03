package com.example.adventofcode

object Day2Part2 extends App {

  object Point {
    val forbidden = Seq(
      (0, 0), (0, 1), (1, 0),
      (3, 0), (4, 0), (4, 1),
      (0, 3), (0, 4), (1, 4),
      (3, 4), (4, 3), (4, 4))
      .map { case (x, y) => Point(x, y) }
  }

  case class Point(x: Int, y: Int) {

    import Point._

    def validate(point: Point) =
      if ((forbidden contains point)
        || (point.x < 0)
        || (point.x > 4)
        || (point.y < 0)
        || (point.y > 4)) this
      else point

    def left = validate(Point(x - 1, y))

    def right = validate(Point(x + 1, y))

    def up = validate(Point(x, y - 1))

    def down = validate(Point(x, y + 1))
  }

  val res = Inputs.day2.lines.toList.scanLeft(Point(0, 2)) { (fst, snd) =>
    snd.foldLeft(fst) { (prev, curr) =>
      curr match {
        case 'L' => prev.left
        case 'R' => prev.right
        case 'U' => prev.up
        case 'D' => prev.down
      }
    }
  }

  println(res.tail)

}
