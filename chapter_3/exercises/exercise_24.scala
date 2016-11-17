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

	def filterOld[A](as: List[A])(f: A => Boolean): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => if(f(x)) filter(xs)(f)
							else Cons(x, filter(xs)(f))
	}	

	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
		flattenList(map(as)(f))
	}

	def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if(f(x)) List(x) else Nil)

	def zipWithInt(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
		case (Nil, Nil) => Nil
		case (Nil, b) => b
		case (a, Nil) => a
		case (Cons(h1, t1), Cons(h2, t2)) => {
			Cons((h1 + h2), zipWithInt(t1,t2))
		}
	}

	def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1, l2) match {
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(h1, t1), Cons(h2, t2)) => Cons((f(h1 ,h2)), zipWith(t1,t2)(f))
	}

	/*

	Hard: As an example, implement hasSubsequence for checking whether a List con- tains another 
	List as a subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) 
	as subsequences, among others. 
	You may have some difficulty finding a concise purely functional implementation 
	that is also efficient. That’s okay. Implement the function however comes most naturally. 
	We’ll return to this implementation in chapter 5 and hopefully improve on it. 
	Note: Any two values x and y can be compared for equality in Scala using the expression x == y.

	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean

	*/

	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

		def loop[A](supRev: List[A], subRev: List[A], isSubSequence: Boolean): Boolean = supRev match {
			case Nil => isSubSequence
			case Cons(x,xs) => subRev match {
				case Nil => isSubSequence
				case Cons(x2, xs2) => {
					if(x == x2) loop(xs, xs2, true)
					else{
						if(isSubSequence == true) loop(xs, xs2, false)
						else loop(xs, subRev, false)
					}
				}
			}
		}
		
		
		loop(reverse(sup), reverse(sub), false)
	}
}

/*
tests
*/

assert(List.hasSubsequence(List(1,2,3,4,5), List(4,5)) == true)
assert(List.hasSubsequence(List(1,2,3,4,5), List(2,3,4)) == true)
assert(List.hasSubsequence(List(1,2,3,4,5), List(4)) == true)
assert(List.hasSubsequence(List(1,2,3,4,5), List(5,4)) == false)
assert(List.hasSubsequence(List(1), List(5,4)) == false)

