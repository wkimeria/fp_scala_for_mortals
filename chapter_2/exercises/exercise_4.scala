/*
Implement uncurry, which reverses the transformation of curry. 
Note that since => associates to the right, A => (B => C) can be written as A => B => C.
*/

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
	a, b) => f(a)(b)

	/* Can also be written as
	(a, b) => {
		val func1 = f(a)
		func1(b)
	}
	*/

}

/*
 - uncurry takes 3 types (A,B,C)
 - Takes a function that takes an argument of type A that returns a function that
   an argument of type B and returns a value of type C
 - Returns a function that takes arguments of type A and B and returns a value of type C

ERR_WUT?
Got stuck on this one, and had to refer back to my solution from last year. I really
don't understand how this syntax makes sense intuitively. I don't understand what f(a)(b) means.

I get that f(a) means apply the function f to the argument a of type A. That will yield
a function that takes an argument b of type B and returns c of type C. 

So it seems the (b) part means immediately apply the function returned to  argumet b of type B in 
order to return c of type C

*/

/*
Tests
*/

val func1 = (a: String, b: String) => a + " " + b

//Curry (from Exercise 2.3)
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
	(a: A) => (b:B) => f(a,b)
}

val curried = curry(func1)
val result1 = curried("Hello")
val result2 = result1("There!")

assert(result2 == "Hello There!")

val uncurried = uncurry(curried)
val result3 = uncurried("Hello", "There again!")

assert(result3 == "Hello There again!")