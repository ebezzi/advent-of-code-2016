package com.example.adventofcode

object Day4Part2 extends App {

  def extract(input: String) = {
    val split = input.split(Array('[', ']')).filter(_.nonEmpty)
    (split.head, split.last)
  }

  def isProper(input: String) = {

    val (data, checksum) = extract(input)

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

  val res  = Inputs.day4.lines.filter(isProper)
    .map { room =>
      val sector = room.filter(_.isDigit).mkString.toInt
      val (data, _) = extract(room)
      val alpha = 'a' to 'z'
      val cipher = data
        .map(char => alpha((alpha.indexOf(char) + sector) % alpha.length) )
        .mkString
      cipher -> sector
    }
    .foreach(println)
}
