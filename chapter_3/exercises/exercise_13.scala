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
		println("foldright as = " + as + " z = " + z)
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs,z)(f))
		}
}

	def sum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

	def product(ns: List[Double]) = foldLeft(ns, 1.0) (_ * _)

	def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => 1 + x)

	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
		println("foldleft as = " + as + " z = " + z)
			as match {
			case Nil => z
			case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
		}
	}

	def reverse[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => foldLeft(xs, Cons(x,Nil))((a,b) => Cons(b, a))
	}

	/*

	Hard: Can you write foldLeft in terms of foldRight? How about the other way around? 
	Implementing foldRight via foldLeft is useful because it lets us implement foldRight 
	tail-recursively, which means it works even for large lists without overflow- ing the stack.

	*/

	def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
		foldLeft(reverse(as), z)((b,a) => f(b,a))

	/*
	* Had to look at the answer for this one. Revisit
	*/

	def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
		foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


}

/*
tests
*/


val list = List(1,2,3,4)

val a = List.foldRightViaFoldLeft(list, 0)((x,y) => x + y)
val b = List.foldLeftViaFoldRight(list, 0)((x,y) => x + y)

assert(a == 10)
assert(b == 10)



/*

Notes. Here is a printout of what is happening

List.foldRight(list, 0)((x,y) => x + y)

foldright as = Cons(1,Cons(2,Cons(3,Cons(4,Nil)))) z = 0
foldright as = Cons(2,Cons(3,Cons(4,Nil))) z = 0
foldright as = Cons(3,Cons(4,Nil)) z = 0
foldright as = Cons(4,Nil) z = 0
foldright as = Nil z = 0

List.foldLeft(list, 0)((x,y) => x + y)

foldleft as = Cons(1,Cons(2,Cons(3,Cons(4,Nil)))) z = 0
foldleft as = Cons(2,Cons(3,Cons(4,Nil))) z = 1
foldleft as = Cons(3,Cons(4,Nil)) z = 3
foldleft as = Cons(4,Nil) z = 6
foldleft as = Nil z = 10

*/
