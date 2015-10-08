// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness


//import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary


import scala.util.Random

// comment out all the lines below to test Scala Standard Library implementation
// of Streams. Interestingly the standard library streams are stricter than
// those from the book, so some laziness tests fail on them :)
//

//import stream._

import stream00._ // uncomment to test the book laziness solution implementation
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecMindaugas extends FlatSpec with Checkers {

  import Stream._


  behavior of "headOption"
  it should "return None on an empty Stream" in {
    assert(empty.headOption == None)
  }

  it should "return Some(n) on non empty stream and not force the tail " in check {
    ("singleton" |: Prop.forAll { (n: Int) => cons(n, throw new RuntimeException("Tail was forced")).headOption == Some(n) }) &&
      ("random" |: Prop.forAll(genNonEmptyStream) { (s: Stream[Int]) => s.headOption != None })
  }


  behavior of "take"
  it should "return empty on an empty stream" in {
    forAll((n: Int) => empty.take(n) == empty)
  }


  it should "not force the head" in {
    assert(runtimeExceptionStream("head was forced").take(5) != empty)
  }

  it should "return s.take(n).take(n) == s.take(n) for any Stream s and any n" in check {

    Prop.forAll(genNonEmptyStream) { (s: Stream[Int]) => s.take(100).take(100).toList == s.take(100).toList }
  }


  behavior of "drop"

  it should "return empty on an empty stream" in {
    forAll((n: Int) => empty.drop(n) == empty)
  }

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m " in check {
    val stream = from(0).take(100)
    val m = Random.nextInt(stream.toList.size / 2)
    val n = Random.nextInt(stream.toList.size / 2)
    // m+n will at most be of stream.size
    Prop.forAll(stream) { (s: Stream[Int]) => s.drop(m).drop(n).toList == s.drop(m + n).toList }
  }

  it should "s.drop(n) does not force any of the dropped elements heads" in {
    assert(runtimeExceptionStream("head was forced").drop(10) != null)
  }

  behavior of "map"

  it should "x.map(id) == x (where id is the identity function)" in check {
    val id = (x: Int) => x
    Prop.forAll(genNonEmptyStream) { (s: Stream[Int]) => s.map(id).toList == s.toList }
  }

  //not completely sure if this is correct
  it should "map terminates on infinite streams" in {
    val id = (x: Int) => x
    assert(from(0).map(id) != null)
  }


  behavior of "append"

  it should "append stream to the end of the another stream" in {
    assert(from(0).take(10).append(from(0).drop(10).take(10)).toList == from(0).take(20).toList)
  }

  // the head is being forced
  it should "not force the head nor the tail" in {
    assert(runtimeExceptionStream("head was forced").take(10).append(throw new RuntimeException("tail was invoked")) != empty)
  }

  // Creates infinite stream of RuntimeExceptions.
  def runtimeExceptionStream(s: String): Stream[RuntimeException] =
    cons(throw new RuntimeException(s), runtimeExceptionStream(s))

  // An example generator of random finite non-empty streams
  def list2stream[A](la: List[A]): Stream[A] = la.foldRight(empty[A])(cons[A](_, _))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A](implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for {la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
      yield list2stream(la)
}


/**
 * We use scenario based testing when we want to run a test on some input defined by the requirements.
 * Usually we test on few data inputs. With property based testing, we run multiple test and on each test
 * we generate new set of inputs. By this we try to find an "edge" on which input the tested unit will crash.
 * In another words, we check the limitations of the unit that we are testing.
 *
 */
