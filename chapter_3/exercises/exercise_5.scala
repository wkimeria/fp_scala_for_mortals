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

	/*
	Implement dropWhile, which removes elements from the List prefix as long as 
	they match a predicate.

	def dropWhile[A](l: List[A], f: A => Boolean): List[A]

	*/
	
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(h, t) => if(f(h) == true) dropWhile(t,f)
					  	   else Cons(h,t)
	}
}


/*
Tests
*/

assert(List.dropWhile(List(1,2,3,4,5), (f:Int)=> (f % 4) != 0) == List(4,5))
assert(List.dropWhile(List("a", "b", "c", "d"), (f:String)=> f != "c") == List("c", "d"))
assert(List.dropWhile(Nil, (f:Int)=> (f % 4) != 0) == Nil)

