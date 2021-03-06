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

	def map[A,B](as: List[A])(f: A => B): List[B] = as match {
		case Nil => Nil
		case Cons(x, xs) => Cons(f(x), map(xs)(f))
	}

	def flattenList[A](as: List[List[A]]): List[A] = {
		as match {
			case Nil => Nil
			case Cons(x,xs) => foldRight(x, flattenList(xs))((a,b) => Cons(a,b))
		}
	}

	/*

	Write a function flatMap that works like map except that the function given will return a list 
	instead of a single result, and that list should be inserted into the final resulting list. 

	Here is its signature:
	
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]

	For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).

	*/

	def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
		as match {
			case Nil => Nil
			case Cons(x, xs: List[A]) => foldRight(foldLeft(xs,f(x))((a,b) => a), flatMap2(xs)(f))((a,b) => Cons(a,b))
		}
	}


	/*

	Had to look at the answers to realize I could do this as below (chaining flattenList and map)

	*/

	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
		flattenList(map(as)(f))
	}
}

/*
tests
*/
assert(List.flatMap(Nil)(i => List(i,i)) == Nil)
assert(List.flatMap(List(1))(i => List(i,i)) == List(1,1))
assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))

assert(List.flatMap2(Nil)(i => List(i,i)) == Nil)
assert(List.flatMap2(List(1))(i => List(i,i)) == List(1,1))
assert(List.flatMap2(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
