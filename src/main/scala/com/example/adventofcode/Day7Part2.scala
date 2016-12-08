package com.example.adventofcode


import scala.io.Source

/**
  * Created by emanuele on 08/12/16.
  */
object Day7Part2 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("/day7.input")).getLines

  def split(xs: String) =
    xs.split("[\\[\\]]")

  def findABA(xs: String) = xs.sliding(3).map(_.toList).collect {
    case s @ a +: b +: c +: Nil if a == c && a != b => Array(b,a,b).mkString
  }.toList

  def hasSSL(xs: String) = {
    val outside = split(xs).zipWithIndex.collect { case (ys, i) if i % 2 == 0 => ys }
    val inside = split(xs).zipWithIndex.collect { case (ys, i) if i % 2 > 0 => ys }
    val abas = outside.flatMap(findABA)
    abas.exists(aba => inside.exists(_ contains aba))
  }

  val res = input.count(hasSSL)
  println(res)

}
