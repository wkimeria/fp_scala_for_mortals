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

	def foldRight[A,B](as: List[A], z: B)(f:(A,B) => B): B = {
		println("FoldRight: as = " + as + " z = " + z)
		val ret = as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs,z)(f))
		}

		println("FoldRight: return = " + ret)

		ret
	}

	def sum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

	def product(ns: List[Double]) = foldLeft(ns, 1.0) (_ * _)

	def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1 + x)

	//@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
		println("FoldLeft: as = " + as + " z = " + z)
		val ret = as match {
			case Nil => z
			case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
		}

		println("FoldLeft: return = " + ret)

		ret
	}



	def sumRight(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

	def sumLeft(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)
}

/*
tests
*/

println(List.sumRight(List(1,2,3,4,5)))
println(List.sumLeft(List(1,2,3,4,5)))
