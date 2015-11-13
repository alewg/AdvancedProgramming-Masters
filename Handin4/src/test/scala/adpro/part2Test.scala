package test.scala.adpro

import adpro.data.Digit
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import language.implicitConversions
import scala.collection.mutable


class part2Test extends FlatSpec with Checkers {

  import adpro.data._
  import adpro.data.FingerTree._

  def fingerTreeOfN[A](n: Int, gen: Gen[A]): Gen[FingerTree[A]] = Gen.listOfN(n, gen).map(Digit.toTree)

  //    behavior of "addR"
  //
  //    it should "create a fingertree after 100 addR" in check {
  //      Prop.forAll(Gen.listOfN(100, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
  //      }
  //    }
  //

  //    it should "create a fingertree after 10000 addR" in check {
  //      Prop.forAll(Gen.listOfN(10000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
  //      }
  //    }
  //
  //    it should "create a fingertree after 1000000 addR" in check {
  //      Prop.forAll(Gen.listOfN(1000000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
  //      }
  //    }
  //
  //  behavior of "addL"
  //
  //  it should "create a fingertree after 100 addL" in check {
  //    Prop.forAll(Gen.listOfN(100, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //      l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
  //    }
  //  }
  //
  //  it should "create a fingertree after 10000 addL" in check {
  //    Prop.forAll(Gen.listOfN(10000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //      l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
  //    }
  //  }
  //
  //  it should "create a fingertree after 1000000 addL" in check {
  //    Prop.forAll(Gen.listOfN(1000000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
  //      l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
  //    }
  //  }
  //
  //  behavior of "popR"
  //
  //  it should "pop 100 nodes fingertree from right" in check {
  //    val tree = fingerTreeOfN(100, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailR(r)
  //        }
  //      };
  //        true
  //    }
  //  }
  //
  //  it should "pop 10000 nodes fingertree from right" in check {
  //    val tree = fingerTreeOfN(10000, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailR(r)
  //        }
  //      };
  //        true
  //    }
  //  }
  //
  //  it should "pop 1000000 nodes fingertree from right" in check {
  //    val tree = fingerTreeOfN(1000000, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailR(r)
  //        }
  //      };
  //        true
  //    }
  //  }
  //
  //  behavior of "popL"
  //
  //  it should "pop 100 nodes fingertree from left" in check {
  //    val tree = fingerTreeOfN(100, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailL(r)
  //        }
  //      };
  //        true
  //    }
  //  }
  //
  //  it should "pop 10000 nodes fingertree from left" in check {
  //    val tree = fingerTreeOfN(10000, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailL(r)
  //        }
  //      };
  //        true
  //    }
  //  }
  //
  //  it should "pop 1000000 nodes fingertree from left" in check {
  //    val tree = fingerTreeOfN(1000000, Gen.choose[Int](0, 1000))
  //    forAll(tree) {
  //      (t: FingerTree[Int]) => {
  //        var r = t
  //        while (r != Empty()) {
  //          r = tailL(r)
  //        }
  //      };
  //        true
  //    }
  //  }


  behavior of "doublelinkedlist"



  behavior of "add right"

  it should "add 100 elements to the right" in check {
    var dll = mutable.DoubleLinkedList.empty[Int]
    forAll { (i: Int) => {
      while (dll.size < 100) {
        dll = dll :+ i
      }
    }; true
    }
  }

  it should "add 10000 elements to the right" in  {
    //to run the test as a property test, uncomment forAll...
    var dll = mutable.DoubleLinkedList.empty[Int]
    //forAll { (i: Int) => {
    while (dll.size < 10000) {
      dll = dll :+ 1
    }
    // }; true
    // }
  }
  it should "add 1000000 elements to the right" in  {
    //to run the test as a property test, uncomment forAll...
    var dll = mutable.DoubleLinkedList.empty[Int]
    // forAll { (i: Int) => {
    while (dll.size < 1000000) {
      dll = dll :+ 1
    }
    // }; true
    //  }
  }
  behavior of "add left"

  it should "add 100 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.empty[Int]
    forAll { (i: Int) => {
      while (dll.size < 100) {
        dll = i+:dll
      }
    }; true
    }
  }

  it should "add 10000 elements to the left" in  {
    var dll = mutable.DoubleLinkedList.empty[Int]
    //forAll { (i: Int) => {
    while (dll.size < 10000) {
      dll = 1+:dll
    }
    // }; true
    // }
  }
  it should "add 1000000 elements to the left" in  {
    var dll = mutable.DoubleLinkedList.empty[Int]
    // forAll { (i: Int) => {
    while (dll.size < 1000000) {
      dll = 1+:dll
    }
    // }; true
    //  }
  }

  behavior of "remove left"

  it should "remove 100 elements to the left" in  {
    var dll = mutable.DoubleLinkedList.range(0,100)
    //forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.drop(1)
    }
    // }; true
    // }
  }

  it should "remove 10000 elements to the left" in  {
    var dll = mutable.DoubleLinkedList.range(0,10000)
    //forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.drop(1)
    }
    // }; true
    // }
  }
  it should "remove 1000000 elements to the left" in  {
    var dll = mutable.DoubleLinkedList.range(0,100000)
    // forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.drop(1)
    }
    // }; true
    //  }
  }

  behavior of "remove right"

  it should "remove 100 elements to the right" in  {
    var dll = mutable.DoubleLinkedList.range(0,100)
    //forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.dropRight(1)
    }
    // }; true
    // }
  }
  it should "remove 10000 elements to the right" in  {
    var dll = mutable.DoubleLinkedList.range(0,10000)
    //forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.dropRight(1)
    }
    // }; true
    // }
  }
  it should "remove 1000000 elements to the right" in  {
    var dll = mutable.DoubleLinkedList.range(0,100000)
    // forAll { (i: Int) => {
    while (dll.size > 0) {
      dll = dll.dropRight(1)
    }
    // }; true
    //  }
  }
}