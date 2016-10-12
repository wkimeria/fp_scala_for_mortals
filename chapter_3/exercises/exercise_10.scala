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

	def length[A](as: List[A]): Int = foldRight(as, 0)((_,y) => 1 + y)

	/*

	Our implementation of foldRight is not tail-recursive and will result in a 
	StackOverflowError for large lists (we say it’s not stack-safe). 
	Convince yourself that this is the case, and then write another general 
	list-recursion function, foldLeft, that is tail-recursive, using the 
	techniques we discussed in the previous chapter. Here is its signature

	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B

	*/

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => {
			foldLeft(xs,f(z,x))(f)
		}
		//case Cons(x, xs) => f(foldLeft(xs,z)(f), x)
	}

	def length2[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1 + x)

}

/*
tests
*/

assert(List.length2(Nil) == 0)
assert(List.length2(List(9)) == 1)
assert(List.length2(List(1,2,3)) == 3)

/*
This is easier to reason if you split the non Nil part of the case expression into 2 parts

val y = f(z,x)
foldLeft(xs,y)(f)

which is equivalent to 

foldLeft(xs,f(z,x))(f)
*/




