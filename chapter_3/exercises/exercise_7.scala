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
	Can product, implemented using foldRight, immediately halt the recursion and 
	return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting 
	might work if you call foldRight with a large list. This is a deeper question that 
	we’ll return to in chapter 5.

	NO: Foldright has to traverse all the way to the end of the list (pushing frames onto the stack), before it can begin to collapse it
	*/
}

/*
tests
*/

assert(List.sum2(List(1,2,3,4)) == 10)
assert(List.product2(List(1,2,3)) == 6)