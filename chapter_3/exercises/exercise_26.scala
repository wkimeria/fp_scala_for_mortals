sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	/*

	Write a function maximum that returns the maximum element in a Tree[Int]. 
	(Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)

	*/

	def maximum(tr: Tree[Int]): Int = {
		def loop(t: Tree[Int], maximum: Int): Int = t match {
			case Leaf(value: Int) => value max maximum
			case  Branch(l, r) => loop(l, maximum) max loop(r, maximum)
		}
		val finalMax = loop(tr, 0)
		finalMax
	}
}

/*
tests
*/

val leftBranch = Branch(Leaf(70), Leaf(20))
val rightBranch = Branch(Leaf(30), Leaf(40))
val tree = Branch(leftBranch, rightBranch)

assert(Tree.maximum(tree) == 70)
assert(Tree.maximum(leftBranch) == 70)
assert(Tree.maximum(rightBranch) == 40)