object ex1 extends App{

def fib(n:Int): Int = {
		def go(n:Int, i:Int, y:Int): Int = 
			if(n==0) y
			else go(n-1, y, i+y)

		if(n==1) 0
		else if(n==2) 1
		else go(n-2,0, 1)
	}

	
	println(fib(4))


}

