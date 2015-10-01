// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala



import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  //Exercise 5.1
  def toList: List[A] = {
  	def go(s:Stream[A], acc:List[A]): List[A] = s match {
  		case Cons(h, t) =>go(t(), h()::acc) 
		case _ => acc
  	}
  	go(this, List()).reverse
  	
  }

  //Exercise 5.2
  def take(n:Int): Stream[A] = this match {
  	case Cons(h, t) if n>1 => cons(h(), t().take(n-1))
  	case Cons(h, _) if n==1 => cons(h(), empty)
  	case _ => empty
  }
  //Exercise 5.2
  def drop(n:Int): Stream[A] = this match {
  	case Cons(_, t) if n==0 => t()
  	case Cons(_, t) if n>0 => t().drop(n-1)
  	case _ => empty
  }

  //Exercise 5.3
  def takeWhile(p: A=>Boolean): Stream[A] = this match {
  	case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
  	case _ => empty 
  }

  //Exercise 5.4
  def forAll(p:A=>Boolean):Boolean = 
  	foldRight(true)((a, b) => p(a) && b)
  

  //Exercise 5.7
  def map[B](p:A=>B): Stream[B] = 
  	foldRight(empty[B])((h, t) => cons(p(h), t))

  def filter(p:A=>Boolean): Stream[A] = 
  	foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else t)

  def append[B >: A](s: =>Stream[B]): Stream[B] = 
  	foldRight(s)((h, t) => cons(h, t))

}
 




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq
}

object ch5 extends App{
	val str = Stream(1, 2, 3 , 4, 5, 6, 7, 8, 9 ,10)

	println(str.take(3).toList)
	println(str.drop(4).toList)
	println(str.takeWhile( x => x <= 5).toList)
	println(str.forAll( x => x < 15))
	println(str.map(x => x+1).toList)
	println(str.filter(x => x>5).toList)
	println(str.append(str).toList)
}

