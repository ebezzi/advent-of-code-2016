package com.example.adventofcode

import scala.io.Source

/**
  * Created by emanuele on 08/12/16.
  */
object Day7 extends App {

//  val input = "abba[mnop]qrst"
  val input = Source.fromInputStream(getClass.getResourceAsStream("/day7.input")).getLines

  def split(xs: String) =
    xs.split("[\\[\\]]")

  def hasABBA(xs: String) = xs.sliding(4).map(_.toList).collect {
    case a +: b +: c +: d +: Nil if a == d && b == c && a != b => true
  }.nonEmpty

  def hasTLS(xs: String) =
    Array(split(xs).zipWithIndex.collect { case (xs, i) if i % 2 == 0 => xs }.exists(hasABBA),
      !split(xs).zipWithIndex.collect { case (xs, i) if i % 2 > 0 => xs }.exists(hasABBA)).forall(identity)

  val res = input.count(hasTLS)
  println(res)

}
