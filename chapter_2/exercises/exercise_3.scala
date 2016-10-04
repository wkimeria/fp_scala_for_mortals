/*
Let’s look at another example, currying, which converts a function f of two arguments into a function of 
one argument that partially applies f. 
Here again there’s only one implementation that compiles. Write this implementation.
*/

def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
	(a: A) => (b:B) => f(a,b)
}

/*
So this is the first section I got confused and had to re-read it aloud. My out loud explanation

- curry works 3 types (A,B,C)
- It takes a function that takes two arguments, one of type A and one of type B.
- It returns a function that takes a type of A and returns a function that takes an argument of type B and
- returns an argument of type C
*/


/*
Tests
*/

val func1 = (a: Int, b: Double) => a * b
val curried = curry(func1)

//Return a partially applied function
val result1 = curried(9)	

val result2 = result1(3)

assert(result2 == 27)
