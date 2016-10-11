/* Leaving out package declaration for convenience */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds:List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x + product(xs)
	}

	def apply[A](as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A](items: List[A]): List[A] = items match{
		case Nil => Nil
		case Cons(h,t) => t
	}

	/*
	Using the same idea as tail, implement the function setHead for 
	replacing the first element of a List with a different value.
	*/
	def setHead[A](items: List[A], n: A): List[A] = items match{
		case Nil => Nil
		case Cons(h,t) => Cons(n, t)
	}
}

/*
Tests
*/

assert(List.setHead(List(1,2,3,4,5), 9) == List(9,2,3,4,5))
assert(List.setHead(Nil,9) == Nil)