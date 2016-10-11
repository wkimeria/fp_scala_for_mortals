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

	def setHead[A](items: List[A], n: A): List[A] = items match{
		case Nil => Nil
		case Cons(h,t) => Cons(n, t)
	}

	def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
		case (Nil, _) => Nil
		case (ls,0) => ls
		case(Cons(h,t), c) => drop(t, c-1)
	}
	
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(h, t) => if(f(h) == true) dropWhile(t,f)
					  	   else Cons(h,t)
	}

	/*
	Not everything works out so nicely. Implement a function, init, that returns a List consisting of all 
	but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t 
	this function be implemented in constant time like tail?

	def init[A](l: List[A]): List[A]

	The reason this cannot be done in constant time is because you have to traverse the whole list 
	to get to the last element. It's messy, and the solution below reverses the list, gets the tail 
	only, and then reverses it again. There is probably a faster more intuitive way of doing this 
	but none that will perform in constant time.
	*/

	def init[A](l: List[A]): List[A] = {
		def reverse(l1: List[A], l2: List[A]):List[A] = {
			val reversed = l1 match{
				case Nil => l2
				case Cons(h, t) => reverse(t,Cons(h,l2))
			}
			return reversed
		}
	
		reverse(List.tail(reverse(l, Nil)), Nil)
	}
}


/*
Tests
*/
assert(List.init(Nil) == Nil)
assert(List.init(List(1)) == Nil)
assert(List.init(List(1,2,3,4)) == List(1,2,3))