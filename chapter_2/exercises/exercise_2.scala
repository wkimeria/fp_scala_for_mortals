/*
Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
*/

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

	@annotation.tailrec
	def loop(n: Int, isSorted: Boolean): Boolean = {
		if(n >= (as.length - 1)) isSorted
		else if ((ordered(as(n), as(n+1))) == true) loop(n+1, true)
		else false
	}
	loop(0, true)
}

/*
Tests
*/

assert(isSorted(Array(1),((a: Int, b: Int) => b > a)) == true)
assert(isSorted(Array(1,2),((a: Int, b: Int) => b > a)) == true)
assert(isSorted(Array(2,1),((a: Int, b: Int) => b > a)) == false)
assert(isSorted(Array(1,2,3),((a: Int, b: Int) => b > a)) == true)
assert(isSorted(Array(1,3,2),((a: Int, b: Int) => b > a)) == false)
assert(isSorted(Array(1,3,2,4,0,3),((a: Int, b: Int) => b > a)) == false)