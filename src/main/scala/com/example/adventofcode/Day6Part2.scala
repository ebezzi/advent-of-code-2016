package com.example.adventofcode

/**
  * Created by emanuele on 08/12/16.
  */
object Day6Part2 extends App {

  val res = Inputs.day6.lines
    .toList
    .transpose
    .map { line => line.groupBy(c => line.count(c == _)) }
    .map(_.minBy { case (k,v) => k })
    .map(_._2.head)
    .mkString

  println(res)

}
