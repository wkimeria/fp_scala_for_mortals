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

	/*
	Generalize tail to the function drop, which removes the first n elements 
	from a list. Note that this function takes time proportional only to 
	the number of elements being dropped—we don’t need to make a copy of 
	the entire List.

	def drop[A](l: List[A], n: Int): List[A]

	*/
	def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
		case (Nil, _) => Nil
		case (ls,0) => ls
		case(Cons(h,t), c) => drop(t, c-1)
	}
}

assert(List.drop(List(1,2,3,4),2) == List(3,4))
assert(List.drop(List(1),1) == Nil)
assert(List.drop(Nil,2) == Nil)
assert(List.drop(List(1,2),0) == List(1,2))