sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	/*

	Write a function size that counts the number of nodes (leaves and branches) in a tree.

	*/
	def size[A](tr: Tree[A]): Int = {
		def loop[A](t: Tree[A]): Int = t match {
			case Leaf(_) => 1
			case  Branch(left, right) => loop(left) + loop(right)
		}
		val count = loop(tr)

		tr match {
			case Leaf(_) => count
			case Branch(_,_) => count + 1
		}
	}
}



val leftBranch = Branch(Leaf(10), Leaf(20))
val rightBranch = Branch(Leaf(30), Leaf(40))
val tree = Branch(leftBranch, rightBranch)

assert(Tree.size(tree) == 5)
assert(Tree.size(leftBranch) == 3)
assert(Tree.size(rightBranch) == 3)
assert(Tree.size(Leaf(10)) == 1)