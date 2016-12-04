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

	def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
		foldLeft(reverse(as), z)((b,a) => f(b,a))

	/*

	Write a function map that generalizes modifying each element in a list while maintain- ing the structure of the list. 
	Here is its signature:

	def map[A,B](as: List[A])(f: A => B): List[B]

	*/

def map[A,B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, Nil:List[B])((t,h) => Cons(f(h),t))

	
}

/*
tests
*/
assert(List.map(Nil)(f => f.toString) == Nil)
assert(List.map(List(1))(f => f.toString) == List("1"))
assert(List.map(List(1,2))(f => f.toString) == List("1", "2"))
