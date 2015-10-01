sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List{
	
	def sum(ints:List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds:List[Double]): Double =  ds match {
		case Nil => 1.0
		case Cons(0.0, _ ) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] = // Variadic function
    	if (as.isEmpty) Nil
    	else Cons(as.head, apply(as.tail: _*))



    //Exercise 3.2
    def tail(list:List[Int]):List[Int] = list match {
    	case Nil => sys.error("tail of empty list") 
    	case Cons(_, t) => t
    }

    //Exercise 3.3
    def setHead(x:Int, list: List[Int]): List[Int] = list match {
    	case Nil => sys.error("setHead on empty list")
    	case Cons(_, t) =>  Cons(x, t)

    }

    //Exercise 3.4
    def drop[A](l:List[A], n:Int):List[A] = 
    	if(n<=0) l
    	else l match{
    		case Nil => Nil
    		case Cons(_,t) => drop(t, n-1) 
    	}
    

    //Exercise 3.5
    def dropWhile[A](l:List[A], f: A=>Boolean):List[A] = l match {
    	case Cons(h, t) if(f(h)) =>  dropWhile(t, f)
    	case _ => l   
    }

    //Exercise 3.6
    def init[A](l:List[A]): List[A] = l match {
    	case Nil => sys.error("setHead on empty list") 
    	case Cons(_,Nil) => Nil
    	case Cons(h, t) => Cons(h, init(t))
    }

    //Exercise 3.7 3.8
    def foldRight[A, B](as:List[A], z:B)(f: (A, B)=>B):B = 
    	as match {
    		case Nil => z
    		case Cons(x, xs) => f(x, foldRight(xs, z)(f)) 
    	}

    def sum2 (ns:List[Int]) = 
    	foldRight(ns, 0) ((x, y)=>x+y)
    	
    def product2(ns:List[Double]) = 
    	foldRight(ns, 1.0)(_*_)
    	
    //Exercise 3.9
    def length[A](as:List[A]): Int = 
    	foldRight(as, 0)((_, y) => y+1 )
    
    
   
    //Exercise 3.10
    def foldLeft[A,B](as:List[A], z:B)(f:(B,A)=>B): B =
    	as match {
    		case Nil => z
    		case Cons(x, xs) => foldLeft(xs, f(z, x))(f) 
    	}
    	
    def length2[A](as:List[A]): Int = 
    	foldLeft(as, 0)((x, y)=> x+1)

    //Exercise 3.11
    def sum3(ns:List[Int]): Int = 
    	foldLeft(ns, 0)(_+_)

    //Exercise 3.11
    def product3(ns: List[Double]): Double = 
    	foldLeft(ns, 1.0) (_*_)

   	//Exercise 3.12
   	def reverse[A](ns:List[A]): List[A] = 
   		foldLeft(ns, List[A]()) ((x,y)=>Cons(y, x)) 
   	
   	//Exercise 3.14
  	def append[A](ns:List[A], r:List[A]):List[A] = 
  		foldRight(ns, r)(Cons(_,_))

  	//Exercise 3.16
  	def transform(ns:List[Int]):List[Int] = 
  		ns match {
  			case Nil => Nil
  			case Cons(h, t) => Cons(h+1, transform(t)) 
  		}

  	//Exercise 3.17
  	def doubleToString(ns:List[Double]): List[String] = 
  		ns match {
  			case Nil => Nil
  			case Cons(h, t) => Cons(h.toString(), doubleToString(t))
  		}
  		
  	//Exercise 3.18
  	def map[A,B](as:List[A])(f: A=>B):List[B] =
  		as match {
  			case Nil => Nil
  			case Cons(h, t) => Cons(f(h), map(t)(f))  
  		}
  		

  	//Exercise 3.19
  	def filter[A] (as:List[A])(f: A=>Boolean): List[A] =
  		as match {
  			case Nil => Nil
  			case Cons(h, t) if(f(h)) => Cons(h, filter(t)(f)) 
  			case Cons(h, t) if(!f(h)) => filter(t)(f)
  		}
  		
  }

object ch3 extends App{
	val l = List(1, 2, 3, 4, 5)
	val d = List(1.0, 2.0, 3.0, 4.0, 5.0)

// Exercise 3.1
	val x = l match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
		case Cons (h, t) => h + List.sum(t)
		case _ => 101 
	}
	
	
	println("sum "+List.sum(l))
	println("x "+x)
	println("tail "+List.tail(l))
	println("setHead "+List.setHead(2, l))
	println("drop "+ List.drop(l, 4))
	println("dropWhile "+List.dropWhile(l, (x:Int)=> x<=3))
	println("init "+List.init(l))
	println("foldRight "+List.foldRight(l, Nil:List[Int])(Cons(_,_)))
	println("length "+List.length(l))
	println("length2 "+List.length2(l))
	println("sum3 "+List.sum3(l))
	println("reverse "+List.reverse(l))
	println("append "+List.append(l,l))
	println("transform "+List.transform(l))
	println("doubleToString "+List.doubleToString(d))
	println("map "+List.map(l)((x) => x+2))
	println("filter "+List.filter(l)((x) => x%2==0))
	println("flatMap "+List.flatMap(List(1,2,3))(i => List(i,i)))
	

}