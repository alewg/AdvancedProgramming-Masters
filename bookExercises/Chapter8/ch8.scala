trait Prop {
	def check: Boolean
	def &&(p:Prop): Prop =  new Prop{
		def check = Prop.this.check && p.check
	}

}

object Prop{
	type SuccessCount = Int
	type FailedCase = String
}

case class Gen[A](sample:State[RNG,A]){

}

object Gen {

	def choose(start:Int, stopExclusion:Int): Gen[A] = {
		Gen(State.sequence)
	}
}

object ch8 extends App{

}