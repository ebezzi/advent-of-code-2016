package com.example.adventofcode

import com.example.adventofcode.Day1.Position

object Day2 extends App {

//  println (Inputs.day2.lines.toSeq.length)

  case class Point(x: Int, y: Int) {
    def left = if (x > 0) Point(x-1, y) else this
    def right = if (x < 2) Point(x+1, y) else this
    def up = if (y > 0) Point(x, y-1) else this
    def down = if (y < 2) Point(x, y+1) else this
  }

//  def computeLine(line: Seq[Char])

  val res = Inputs.day2.lines.toList.scanLeft(Point(1,1)) { (fst, snd) =>
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
