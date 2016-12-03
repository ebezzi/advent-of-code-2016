package com.example.adventofcode

object Day3Part2 extends App {

  def isTriangle(dim: Seq[Int]): Boolean =
    dim.permutations.forall( x => x(0) + x(1) > x(2))

  val columns = Inputs.day3.lines
    .toSeq
    .map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt))

  val res = (columns.map(_(0)) ++ columns.map(_(1)) ++ columns.map(_(2))).sliding(3,3).count(isTriangle)

  println(res)

}
