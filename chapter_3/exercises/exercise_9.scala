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
	Compute the length of a list using foldRight. 

	def length[A](as: List[A]): Int
	*/

	def length[A](as: List[A]): Int = foldRight(as, 0)((_,y) => 1 + y)

}

/*
tests
*/

assert(List.length(Nil) == 0)
assert(List.length(List(9)) == 1)
assert(List.length(List(1,2,3)) == 3)