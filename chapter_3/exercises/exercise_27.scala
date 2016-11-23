sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	/*

	Write a function depth that returns the maximum path length from the 
	root of a tree to any leaf.

	*/

	def depth(tr: Tree[Int], value: Int): Int = {
		def loop(t: Tree[Int], depth: Int): Int = t match {
			case Leaf(v: Int) => if(v == value) depth else 0
			case  Branch(l, r) => loop(l, depth + 1) max loop(r, depth + 1)
		}
		loop(tr, 0)
	}
}

/*
tests
*/

val rightBranch = Branch(Leaf(30), Branch(Leaf(70), Leaf(20)))
val tree = Branch(Leaf(33), rightBranch)

assert(Tree.depth(tree, 70) == 3)
assert(Tree.depth(tree, 30) == 2)
assert(Tree.depth(tree, 33) == 1)
assert(Tree.depth(tree, 101) == 0) //Not found
