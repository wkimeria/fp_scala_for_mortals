sealed trait Option[+A]{
	def map[B](f: A => B): Option[B] = {
		this match {
			case None => None
			case Some(x) => Some(f(x))
		}
	}

	//TODO: Return here and implement without using pattern matching
	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(x) => f(x)
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(x) => x
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this flatMap((f) => ob)

	def filter(f: A => Boolean): Option[A] = {
		this.flatMap((x) => {
				if(f(x) == true) Some(x)
				else None
			}
		)
	}

	def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

/*
Write a generic function map2 that combines two Option values using a binary function. 
If either Option value is None, then the return value is too. Here is its signature:

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]

*/

//TODO: Can I do this without pattern matching?
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]  = {
	(a,b) match {
		case (None, _) => None
		case (_, None) => None
		case (Some(x), Some(y)) => Some(f(x,y))
	}

}

assert(map2(Some(1), Some(2)) ((a,b) => a + b) == Some(3))
assert(map2(Some(1), None) ((a,b) => a + b) == None)
assert(map2(None, Some(2)) ((a: Int,b: Int) => a + b) == None)
assert(map2(None, None) ((a: Int,b: Int) => a + b) == None)

