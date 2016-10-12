/* Leaving out package declaration for convenience */
/* collapse other methods for readability */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	
	def apply[A](as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A](items: List[A]): List[A] = ???

	def setHead[A](items: List[A], n: A): List[A] = ???

	def drop[A](l: List[A], n: Int): List[A] = ???
	
	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = ???

	def foldRight[A,B](as: List[A], z: B)(f:(A,B) => B): B = ???

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => {
			foldLeft(xs,f(z,x))(f)
		}
	}

	/*
	
	Write sum, product, and a function to compute the length of a 
	list using foldLeft.

	*/

	def sum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

	def product(ns: List[Double]) = foldLeft(ns, 1.0) (_ * _)

	def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1 + x)
}

/*
tests
*/

assert(List.sum(Nil) == 0)
assert(List.sum(List(1,2,3)) == 6)

assert(List.product(Nil) == 1.0)
assert(List.product(List(1,2,3,4)) == 24)

assert(List.length(Nil) == 0)
assert(List.length(List(1,2,3)) == 3)
