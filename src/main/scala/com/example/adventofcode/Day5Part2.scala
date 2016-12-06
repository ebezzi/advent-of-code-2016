package com.example.adventofcode

import java.security.MessageDigest

object Day5Part2 extends App{

  val input = Inputs.day5

  val digest = MessageDigest.getInstance("MD5")

  def md5(xs: String) =
    digest.digest(xs.getBytes).map("%02x".format(_)).mkString

  Stream.from(1)
    .map(i => md5(s"$input$i"))
    .filter(_.take(5) == "00000")
    .filter(_(5).isDigit)
    .filter(_(5).toString.toInt < 8)
    .scanLeft((Seq.empty[Int], Option[String](null))) { case ((positions, res), h) =>
      val pos = h(5).toString.toInt
      val digit = h(6)
      if (positions contains pos)
        positions -> None
      else
        (positions :+ pos) -> Option((Seq.fill(pos)("_") ++ digit.toString ++ Seq.fill(7-pos)("_")).mkString)
    }
    .collect { case (_, Some(xs)) => xs }
    .take(8)
    .foreach(println)

}
