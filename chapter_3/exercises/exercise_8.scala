/* Leaving out package declaration for convenience */
/* collapse other methods for readability */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ???

	def product(ds:List[Double]) = ???

	def apply[A](as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A](items: List[A]): List[A] = ???

	def setHead[A](items: List[A], n: A): List[A] = ???

	def drop[A](l: List[A], n: Int): List[A] = ???
	
	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = ???

	def foldRight[A,B](as: List[A], z: B)(f:(A,B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs,z)(f))
	}

	def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) = foldRight(ns, 1.0) (_ * _)

	/*
	See what happens when you pass Nil and Cons themselves to foldRight, 
	like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) 
	What do you think this says about the relationship between foldRight and the 
	data constructors of List?

	Answer: The data constructor of List is a function that operates similar to
	        the other functions that can be generalized for use with foldRight.
	        ERR_WUT: Not exactly sure what this question means. Revisit
	*/

}

/*
tests
*/

assert(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == List(1,2,3))

