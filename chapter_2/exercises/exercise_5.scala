/*

Let’s look at a final example, function composition, which feeds the output of one 
function to the input of another function. Again, the implementation of this function is fully 
determined by its type signature.

Implement the higher-order function that composes two functions.
*/

def compose[A,B,C](f: B => C, g: A => B): A => C = {
	(a) => f(g(a))
}

/*
Compose takes arguments of type A, B and C

The first argument is a function f that takes an argument of type B and returns a result of type C
The second argument is a function  g that takes an argument of type A and returns a result of type B
The function return a function that takes an argument of type A and returns a result of type C

This one is pretty straightforward, especially after the  uncurrying exercise (exercise 2.4)
*/

/*
Tests
*/

val func1 = (a: Int) => a.toString()
val func2 = (b:String) => b + " times"
val composed = compose[Int, String, String](func2,func1)

val result = composed(33)

assert(result == "33 times")



