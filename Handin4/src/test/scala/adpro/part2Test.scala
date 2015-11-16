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

    behavior of "FingerTree"
      behavior of "addR"

    it should "create a fingertree after 10 addR" in check {
      Prop.forAll(Gen.listOfN(10, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
      }
    }

    it should "create a fingertree after 100 addR" in check {
      Prop.forAll(Gen.listOfN(100, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
      }
    }


    it should "create a fingertree after 1000 addR" in check {
      Prop.forAll(Gen.listOfN(1000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
      }
    }

    it should "create a fingertree after 10000 addR" in check {
      Prop.forAll(Gen.listOfN(10000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldLeft[FingerTree[Int]](Empty())(FingerTree.addR) != Empty()
      }
    }

    behavior of "addL"
    it should "create a fingertree after 10 addL" in check {
      Prop.forAll(Gen.listOfN(10, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
      }
    }

    it should "create a fingertree after 100 addL" in check {
      Prop.forAll(Gen.listOfN(100, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
      }
    }

    it should "create a fingertree after 1000 addL" in check {
      Prop.forAll(Gen.listOfN(1000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
      }
    }

    it should "create a fingertree after 10000 addL" in check {
      Prop.forAll(Gen.listOfN(10000, Gen.choose[Int](0, 1000))) { (l: List[Int]) =>
        l.foldRight[FingerTree[Int]](Empty())(FingerTree.addL) != Empty()
      }
    }

    behavior of "popR"

    it should "pop 10 nodes fingertree from right" in check {
      val tree = fingerTreeOfN(10, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailR(r)
          }
        };
          true
      }
    }
    it should "pop 100 nodes fingertree from right" in check {
      val tree = fingerTreeOfN(100, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailR(r)
          }
        };
          true
      }
    }

    it should "pop 1000 nodes fingertree from right" in check {
      val tree = fingerTreeOfN(1000, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailR(r)
          }
        };
          true
      }
    }

    it should "pop 10000 nodes fingertree from right" in check {
      val tree = fingerTreeOfN(10000, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailR(r)
          }
        };
          true
      }
    }

    behavior of "popL"

    it should "pop 10 nodes fingertree from left" in check {
      val tree = fingerTreeOfN(10, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailL(r)
          }
        };
          true
      }
    }
    it should "pop 100 nodes fingertree from left" in check {
      val tree = fingerTreeOfN(100, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailL(r)
          }
        };
          true
      }
    }

    it should "pop 1000 nodes fingertree from left" in check {
      val tree = fingerTreeOfN(1000, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailL(r)
          }
        };
          true
      }
    }

    it should "pop 10000 nodes fingertree from left" in check {
      val tree = fingerTreeOfN(10000, Gen.choose[Int](0, 1000))
      forAll(tree) {
        (t: FingerTree[Int]) => {
          var r = t
          while (r != Empty()) {
            r = tailL(r)
          }
        };
          true
      }
    }


    behavior of "Doublelinkedlist"
    behavior of "add right"

    it should "add 10 elements to the right" in check{
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 10) {
          dll = dll :+ i
        }
      }; true
      }
    }
    it should "add 100 elements to the right" in check{
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 100) {
          dll = dll :+ i
        }
      }; true
      }
    }

    it should "add 1000 elements to the right" in  check{
      //to run the test as a property test, uncomment forAll...
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 1000) {
          dll = dll :+ i
        }
      }; true
      }
    }
    //  it should "add 10000 elements to the right" in  {
    //    //to run the test as a property test, uncomment forAll...
    //    var dll = mutable.DoubleLinkedList.empty[Int]
    //    // forAll { (i: Int) => {
    //    while (dll.size < 10000) {
    //      dll = dll :+ 1
    //    }
    //    // }; true
    //    //  }
    //  }
    behavior of "add left"
    it should "add 10 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 10) {
          dll = i +: dll
        }
      };
        true
      }
    }
    it should "add 100 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 100) {
          dll = i+:dll
        }
      }; true
      }
    }

    it should "add 1000 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.empty[Int]
      forAll { (i: Int) => {
        while (dll.size < 1000) {
          dll = i+:dll
        }
      }; true
      }
    }
    //  it should "add 10000 elements to the left" in  {
    //    var dll = mutable.DoubleLinkedList.empty[Int]
    //    // forAll { (i: Int) => {
    //    while (dll.size < 10000) {
    //      dll = 1+:dll
    //    }
    //    // }; true
    //    //  }
    //  }

    behavior of "remove left"
    it should "remove 10 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.range(0,10)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.drop(1)
        }
      }; true
      }
    }
    it should "remove 100 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.range(0,100)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.drop(1)
        }
      }; true
      }
    }

    it should "remove 1000 elements to the left" in check {
      var dll = mutable.DoubleLinkedList.range(0,1000)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.drop(1)
        }
      }; true
      }
    }
    //  it should "remove 10000 elements to the left" in  {
    //    var dll = mutable.DoubleLinkedList.range(0,1000)
    //    // forAll { (i: Int) => {
    //    while (dll.size > 0) {
    //      dll = dll.drop(1)
    //    }
    //    // }; true
    //    //  }
    //  }

    behavior of "remove right"
    it should "remove 10 elements to the right" in check {
      var dll = mutable.DoubleLinkedList.range(0,10)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.dropRight(1)
        }
      }; true
      }
    }
    it should "remove 100 elements to the right" in check {
      var dll = mutable.DoubleLinkedList.range(0,100)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.dropRight(1)
        }
      }; true
      }
    }
    it should "remove 1000 elements to the right" in  check{
      var dll = mutable.DoubleLinkedList.range(0,1000)
      forAll { (i: Int) => {
        while (dll.size > 0) {
          dll = dll.dropRight(1)
        }
      }; true
      }
    }
    //  it should "remove 10000 elements to the right" in  {
    //    var dll = mutable.DoubleLinkedList.range(0,1000)
    //    // forAll { (i: Int) => {
    //    while (dll.size > 0) {
    //      dll = dll.dropRight(1)
    //    }
    //    // }; true
    //    //  }
    //  }


  behavior of "string fingertree"
  val sS = scala.util.Random.nextString(10)
  val sM = scala.util.Random.nextString(1000)
  val sL = scala.util.Random.nextString(1000000)

  it should "add left and right 10 strings of length 10 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 10) {
        ft = ft.addL(sS)
      }
      for (i <- 1 to 10) {
        ft = tailL(ft)
      }
      for (i <- 1 to 10) {
        ft = ft.addR(sS)
      }
      for (i <- 1 to 10) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  it should "add left and right 10 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 10) {
        ft = ft.addL(sM)
      }
      for (i <- 1 to 10) {
        ft = tailL(ft)
      }
      for (i <- 1 to 10) {
        ft = ft.addR(sM)
      }
      for (i <- 1 to 10) {
        ft = tailR(ft)
      }
    };
      true
    }
  }
  it should "add left and right 10 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 10) {
        ft = ft.addL(sL)
      }
      for (i <- 1 to 10) {
        ft = tailL(ft)
      }
      for (i <- 1 to 10) {
        ft = ft.addR(sL)
      }
      for (i <- 1 to 10) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  it should "add left and right 100 strings of length 10 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 100) {
        ft = ft.addL(sS)
      }
      for (i <- 1 to 100) {
        ft = tailL(ft)
      }
      for (i <- 1 to 100) {
        ft = ft.addR(sS)
      }
      for (i <- 1 to 100) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  it should "add left and right 100 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 100) {
        ft = ft.addL(sM)
      }
      for (i <- 1 to 100) {
        ft = tailL(ft)
      }
      for (i <- 1 to 100) {
        ft = ft.addR(sM)
      }
      for (i <- 1 to 100) {
        ft = tailR(ft)
      }
    };
      true
    }
  }
  it should "add left and right 100 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 100) {
        ft = ft.addL(sL)
      }
      for (i <- 1 to 100) {
        ft = tailL(ft)
      }
      for (i <- 1 to 100) {
        ft = ft.addR(sL)
      }
      for (i <- 1 to 100) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  it should "add left and right 1000 strings of length 10 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 1000) {
        ft = ft.addL(sS)
      }
      for (i <- 1 to 1000) {
        ft = tailL(ft)
      }
      for (i <- 1 to 1000) {
        ft = ft.addR(sS)
      }
      for (i <- 1 to 1000) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  it should "add left and right 1000 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 1000) {
        ft = ft.addL(sM)
      }
      for (i <- 1 to 1000) {
        ft = tailL(ft)
      }
      for (i <- 1 to 1000) {
        ft = ft.addR(sM)
      }
      for (i <- 1 to 1000) {
        ft = tailR(ft)
      }
    };
      true
    }
  }
  it should "add left and right 1000 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var ft = addL("0", Empty())
      for (i <- 1 to 1000) {
        ft = ft.addL(sL)
      }
      for (i <- 1 to 1000) {
        ft = tailL(ft)
      }
      for (i <- 1 to 1000) {
        ft = ft.addR(sL)
      }
      for (i <- 1 to 1000) {
        ft = tailR(ft)
      }
    };
      true
    }
  }

  behavior of "string doubleLinkedlist"

  it should "add left and right 10 strings of length 10 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      for (i <- 1 to 10) {
        dl = sS+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      for (i <- 1 to 10) {
        dl = dl:+sS
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }

  it should "add left and right 10 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<10){
        dl = sM+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<10){
        dl = dl:+sM
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }
  it should "add left and right 10 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<10){
        dl = sL+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<10){
        dl = dl:+sL
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }

  it should "add left and right 100 strings of length 10 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<100){
        dl = sS+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<100){
        dl = dl:+sS
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }

  it should "add left and right 100 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<100){
        dl = sM+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<100){
        dl = dl:+sM
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }
  it should "add left and right 100 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<100){
        dl = sL+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<100){
        dl = dl:+sL
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }

  it should "add left and right 1000 strings of length 10 and remove them" in  {

   // forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<1000){
        dl = sS+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<1000){
        dl = dl:+sS
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
   // };
    //  true
  //  }
  }

  it should "add left and right 1000 strings of length 1000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<1000){
        dl = sM+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<1000){
        dl = dl:+sM
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }
  it should "add left and right 1000 strings of length 100000 and remove them" in check {

    forAll { (i: Int) => {
      var i = 0
      var dl = mutable.DoubleLinkedList.empty[String]
      while(dl.size<1000){
        dl = sL+:dl
      }
      while(dl.size>0) {
        dl = dl.drop(1)
      }
      while(dl.size<1000){
        dl = dl:+sL
      }
      while(dl.size>0) {
        dl = dl.dropRight(1)
      }
    };
      true
    }
  }



}



