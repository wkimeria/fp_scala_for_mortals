sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	/*

	Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
	*/

	def map[A](tr: Tree[A])( f: A => A): Tree[A] = tr match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}
}

/*
tests
*/

val leftBranch = Branch(Leaf(70), Leaf(20))
val rightBranch = Branch(Leaf(30), Leaf(40))
val tree = Branch(leftBranch, rightBranch)

assert(Tree.map(Leaf(10))((x: Int) => x + 1) == Leaf(11))
assert(Tree.map(tree)((x: Int) => x + 1) == Branch(Branch(Leaf(71),Leaf(21)),Branch(Leaf(31),Leaf(41))))
assert(Tree.map(tree)((x: Int) => x * x) == Branch(Branch(Leaf(4900),Leaf(400)),Branch(Leaf(900),Leaf(1600))))
