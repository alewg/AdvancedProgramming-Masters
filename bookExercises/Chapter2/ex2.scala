object ex2 extends App{

	def isSorted[A](as:Array[A], ordered:(A,A)=> Boolean): Boolean = {

		def go(i:Int): Boolean ={
			if(as.length == 1) {
				true
			}else if(as.length>1){
				if(i==as.length-1) {
					true
				}else{ 
					if(ordered(as(i), as(i+1))) {
						go(i+1)
					}else {
						false
					}
				}
			}else{ 
				false
			}
		}
		go(0)
	}

	val arr = Array(2, 1, 3, 4, 5)

	println(isSorted(arr, (x:Int, y:Int) => x<=y ))
}