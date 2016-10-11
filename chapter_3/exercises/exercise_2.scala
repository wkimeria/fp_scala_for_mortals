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

	/*
	Implement the function tail for removing the first element of a List. 
	Note that the function takes constant time. What are different choices 
	you could make in your implementation if the List is Nil? 
	We’ll return to this question in the next chapter.
	*/
	def tail[A](items: List[A]): List[A] = items match{
		case Nil => Nil
		case Cons(h,t) => t
	}

}

/*
Tests
*/

assert(List.tail(List(1,2,3,4,5)) == List(2,3,4,5))
assert(List.tail(Nil) == Nil)