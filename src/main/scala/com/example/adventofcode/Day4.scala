package com.example.adventofcode

object Day4 extends App {

  def isProper(input: String) = {

    val split = input.split(Array('[', ']')).filter(_.nonEmpty)
    val (data, checksum) = (split.head, split.last)

    val calculated = data
      .filter(_.isLetter)
      .groupBy(c => data.count(_ == c))
      .mapValues(_.toList.sorted)
      .toList
      .sortBy(-_._1)
      .flatMap(_._2)
      .distinct
      .take(5)
      .mkString

    calculated == checksum

  }

  val res  = Inputs.day4.lines.filter(isProper).map(_.filter(_.isDigit).mkString.toInt).sum
  println(res)

}
