
trait Monoid[A] {
	def op(a1:A, a2:A):A
	def zero: A
}


object Monoid{

	val stringMonoid = new Monoid[String]{
		def op(a1:String, a2:String) = a1 + a2
		val zero = " "
	}

	def listMonoid[A] = new Monoid[List[A]] {
		def op(a1: List[A], a2: List[A]) = a1 ++ a2
		val zero = Nil
	}

	val intAddition: Monoid[Int] = new Monoid[Int]{
		def op(x:Int, y:Int) = x + y
		val zero = 0
	}

	val intMultiplication: Monoid[Int] = new Monoid[Int]{
		def op(x:Int, y:Int ) = x * y
		val zero = 1
	}

	val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
		def op(x:Boolean, y:Boolean) = x || y
		val zero = false
	}

	val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
		def op(x:Boolean, y:Boolean) = x && y
		val zero = true
	}

	def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
		def op(x:Option[A], y:Option[A]) = x orElse y
		val zero = None
	}

	def endMonoid[A]: Monoid[A=>A] = new Monoid[A=>A] {
		def op(f:A=>A, g:A=>A) = f compose g
		val zero = (a:A) => a
	}

	def foldMap[A,B](as:List[A], m:Monoid[B])(f:A => B): B = {
		as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
	}

	def foldMapV[A,B](v:IndexSeq[A], m:Monoid[B])(f:A => B):B = {
		if(as.size == 0)
			m.zero
		else if(as.size == 1)
			f(as(0))
		else 
			val (l, r) as.splitAt(as.size/2)
			op(foldMapV(l, m)(f), foldMapV(r, m)(f))
	}



	sealed trait WC
	case class Stub(chars: String) extends WC
	case class Part(lStub: String, words: Int, rStub: String) extends WC

	val wcMonoid: Monoid[WC] = new Monoid[WC] {
		val zero = Stub("")

		def op(x:WC, y:WC) = (x, y) match{
			case (Stub(c), Stub(d)) => Stub(c+d)
			case (Stub(c), Part(l, w, r)) => Part(c+l, w, r)
			case (Part(l, w, r), Stub(d)) => Part(l, w, r+d)
			case (Part(l1, w, r1), Part(l2, w2, r2)) => Part(l1, w1 +(if((r1 + l2).isEmpty) 0 else 1) +w2, r2)		
		}
	}

	def count(s:String): Int = {
		
	}
}