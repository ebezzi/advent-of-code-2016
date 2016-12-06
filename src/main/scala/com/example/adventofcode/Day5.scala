package com.example.adventofcode

import java.security.MessageDigest

object Day5 extends App{

  val input = Inputs.day5

  val digest = MessageDigest.getInstance("MD5")

  def md5(xs: String) =
    digest.digest(xs.getBytes).map("%02x".format(_)).mkString

  Stream.from(1).map(i => md5(s"$input$i")).filter(_.take(5) == "00000").take(8).foreach(println)

}
