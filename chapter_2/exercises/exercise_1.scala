/*
Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). 
The first two Fibonacci numbers are 0 and 1. 
The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5. 
Your definition should use a local tail-recursive function.
*/
def fib(n: Int): Int = {

	@annotation.tailrec
	def loop(c:Int, a: Int, b: Int): Int = {
		if(c == n) a
		else loop(c + 1, b, a + b)
	}
	loop(0, 0, 1)
}

/*
Tests
*/
assert(fib(0) == 0)
assert(fib(1) == 1)
assert(fib(2) == 1)
assert(fib(3) == 2)
assert(fib(4) == 3)
assert(fib(5) == 5)
assert(fib(6) == 8)



