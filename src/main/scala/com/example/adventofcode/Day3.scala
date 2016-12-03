package com.example.adventofcode

object Day3 extends App {

  def isTriangle(xs: String): Boolean =
    isTriangle(xs.split("\\s+").filter(_.nonEmpty).map(_.toInt))

  def isTriangle(dim: Seq[Int]): Boolean =
    dim.permutations.forall( x => x(0) + x(1) > x(2))

  val res = Inputs.day3.lines.count(isTriangle)

  println(res)

}
