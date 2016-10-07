# Chapter 2 Notes
Chapter 2 is is pretty straightforward. The only confusing part is understanding currying and uncurrying. I love this explanation of currying from the book

>If I can give you a carrot for an apple and a banana, and you already gave me an apple, you just have to give me a banana and I'll give you a carrot.

Currying was rather straightforward. Uncurrying, not so much. The exercise that I spent the most time on in this chapter was exercise 2.5 (Uncurrying).

```scala
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
	(a, b) => f(a)(b)
}
```

For me the difficulty came in wrapping my head around the syntax that I needed to do what I needed to do. The type signature is pretty straightfoward. The talkthrough of exactly what is going on is;

* uncurry is a fuction that deals with arguments of type A, B and C
* t takes a function that takes an argument of type A and returns a function that takes an argument of type B and returns a result of type C
* It returns a function that takes 2 arguments, of type A and type B, and returns a result of type C

I'm having a hard time understanding the utility of uncurry (I can see the utility of curry). Also, the syntax for the body of the method looks a bit weird (for someone not familiar with scala). Essentially it reads 

> "Apply the function f to the argument a of type A, and then apply the function returned to the argument b of type B"

It reads easier when written out as

```scala
(a, b) => {
	val func1 = f(a)
	func1(b)
}
```

Which looks more natural to me. But the preferred style is the shorthand form

```scala
(a, b) => f(a)(b)
```