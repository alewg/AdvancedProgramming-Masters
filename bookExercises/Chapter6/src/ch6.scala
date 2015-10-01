/**
 * Created by mindaugas on 9/9/15.
 */

object ch6 extends App {
  val rng = SimpleRNG(18234);
  val (n1, rng2) = rng.nextInt
  println(n1 + "\n" + rng2)

  val (n2, rng3) = rng.nonNegativeInt(rng)
  println(n2 + "\n"+rng3)

  val (n3, rng4) = rng.double(rng)
  println(n3 + "\n"+rng4)
}

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def double(rng:RNG):(Double, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, ran) = rng.nextInt
    n match {
      case Int.MinValue => (Int.MaxValue, ran)
      case x if x < 0 => (Math.abs(x), ran)
      case x if x >= 0 => (x, ran)
    }
  }

  def double(rng:RNG): (Double, RNG) = {
    val (n, ran) = nonNegativeInt(rng)
    (n/(Int.MaxValue.toDouble+1),ran)


  }
}

