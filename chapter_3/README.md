# Chapter 3 Notes
This was a tough chapter, with a lot of exercises (some mind bending). The chapter focused on Functional Data Structures, which are Immutable, but the biggest concepts for me (both in terms of impact and difficulty) were leftFold and rightFold. Using the ubiquitous Singly linked list was a stroke of genius, and the class definition for List introduced a number of Scala concepts, some of them having equivalents in other non-functional programming languages (i.e Java) and others that are totally new. Here are my rough notes

- trait = Abstract class (rather than an Interface, cause a Trait can contain implementation of methods
- case object = ?
- case class = ?

We also get introduced to data constructors, which are equivalent (I think) to class constructors. The type annotations are pretty straightforward (similar to Java syntax), though I don’t fully understand Scala type inference and that bit me a few times in the exercises.

The chapter also introduced pattern matching, which is very elegant and intuitive.

I was humming along happily working through the exercises and understanding each concept as it was progressively introduced

* High Order Functions
* RightFold and LeftFold (as well as understanding why rightFold is not tail recursive while leftFold is)

My first serious difficulty arrived with exercise 12

*"Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
See if you can write it using a fold."*

I spent an inordinate amount of time on this, until I realized that the 2nd argument to foldLeft can also be a list, rather than a single value. I’m embarrassed by how much time it took me to get myself unstuck, and how obvious the solution was once I realized this.

I skipped exercise 13 and came back to it later

*"Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
Implementing foldRight via foldLeft is useful because it lets us implement foldRight
tail-recursively, which means it works even for large lists without overflow- ing the stack."*

From there, the exercises got progressively harder, but following the rule that every exercise built on the previous one, I made it through (implementing map, flatMap, filter was fun). The last exercises, which dealt with a Tree structure rather than a list, showed how easy it was to generalize the concepts developed earlier in the chapter.

The one exercise I threw the towel in on was Exercise 13, implementing foldLeft via foldRight. Implementing foldRight via FoldLeft was relatively easy (ended up reversing the list prior to calling foldLeft). But foldLeft via foldRight totally stumped me, and even after looking at the solution I’m still confused. I’m definitely coming back to revisit that exercise.

So what concepts do I feel comfortable with, and which ones am I a bit sketchy on?

#### Comfortable
- Data Constructors
- Variadic function syntax
- Composing functions (including separating inputs params to facilitate currying)
- Pattern matching