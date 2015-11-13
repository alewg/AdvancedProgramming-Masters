package main.scala.adpro

import adpro.data.{Digit, FingerTree}
import org.scalacheck.Gen

/**
  * Created by mindaugas on 11/12/15.
  */
object part2 extends App {

  def fingerTreeOfN[A](n: Int, gen: Gen[A]): Gen[FingerTree[A]] = Gen.listOfN(n, gen).map(Digit.toTree)

  val sft = fingerTreeOfN(100, Gen.choose(0, 100)).toString
  val mft = fingerTreeOfN(10000, Gen.choose(0, 100))
  val lft = fingerTreeOfN(1000000, Gen.choose(0, 100))

  println(sft)
}
