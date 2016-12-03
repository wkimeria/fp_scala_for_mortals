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

	def foldRight[A,B](as: List[A], z: B)(f:(A,B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs,z)(f))
	}

	def sum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

	def product(ns: List[Double]) = foldLeft(ns, 1.0) (_ * _)

	def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1 + x)

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
	}

	def reverse[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => foldLeft(xs, Cons(x,Nil))((a,b) => Cons(b, a))
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)((a,b) => Cons(a, b))

	/*

	Hard: Write a function that concatenates a list of lists into a single list. 
	Its runtime should be linear in the total length of all lists. 
	Try to use functions we have already defined.
	*/

	def flattenList[A](as: List[List[A]]): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => append(x,flattenList(xs))
	}
}

/*
tests
*/
assert(List.flattenList(List(Nil, Nil)) == Nil)
assert(List.flattenList(List(List(1), Nil)) == List(1))
assert(List.flattenList(List(List(1))) == List(1))
assert(List.flattenList(List(List(1,2),List(3), List(4,5), List(6,7,8))) == List(1,2,3,4,5,6,7,8))
