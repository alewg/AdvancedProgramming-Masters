// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  def sqr(n: Int): Int = n*n 

  def pow(x:Double)(n:Int): Double = 
  		if((n % 2) == 0 && n > 0) pow(x)(n/2) * pow(x)(n/2)
  		else if((n % 2)!= 0 && n > 0) x * pow(x)(n-1)
  		else if(n == 0) 1
  		else 1/(pow (x)(abs(n)))


  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  private def square(x: Int) = 
  	s"The square value of $x is ${sqr (x)}" 

  private def power(x:Double)(n:Int) = 
  	s"The ${x} power of ${n} is ${pow(x)(n)}"
  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))
    println(square(9))
    println(power(2)(2))
  }
}
