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

	/*

	Write a function filter that removes elements from a list unless they satisfy a given predicate. 
	Use it to remove all odd numbers from a List[Int].

	def filter[A](as: List[A])(f: A => Boolean): List[A]

	*/

	def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => if(f(x)) filter(xs)(f)
							else Cons(x, filter(xs)(f))
	}	
}

/*
tests
*/

assert(List.filter(Nil)((x: Int) => x % 2 == 0) == Nil)
assert(List.filter(List(1,2,3,4,5))((x: Int) => x % 2 == 0) == List(1,3,5))
