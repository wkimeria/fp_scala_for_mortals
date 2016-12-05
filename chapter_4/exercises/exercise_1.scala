/*
Implement all of the preceding functions on Option. As you implement each function, 
try to think about what it means and in what situations you’d use it. We’ll explore 
when to use each of these functions next. Here are a few hints for solving this exercise:
  
- It’s fine to use pattern matching, though you should be able to implement all the 
   functions besides map and getOrElse without resorting to pattern matching.   

- For map and flatMap, the type signature should be enough to determine the
  implementation.

-  getOrElse returns the result inside the Some case of the Option, or if the Option
   is None, returns the given default value.
  
 - orElse returns the first Option if it’s defined; otherwise, it returns the second
    Option.
*/
sealed trait Option[+A]{
	def map[B](f: A => B): Option[B] = {
		this match {
			case None => None
			case Some(x) => Some(f(x))
		}
	}

	//TODO: Return here and implement without using pattern matching
	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case None => None
		case Some(x) => f(x)
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(x) => x
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this flatMap((f) => ob)

	def filter(f: A => Boolean): Option[A] = {
		this.flatMap((x) => {
				if(f(x) == true) Some(x)
				else None
			}
		)
	}
}

case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

/*
tests
*/

//Test for map
val none1: Option[Int] = None
val some1: Option[Int] = Some(4)

assert(none1.map((x) => x * x) == None)
assert(some1.map((x) => x * x) == Some(16))

//Test for flatMap
val none2: Option[Int] = None
val some2: Some[Int] = Some(3)
val result2a = none2.flatMap((x) => Some(x))
val result2b = some2.flatMap((x) => Some(x))

assert(none2.flatMap((x) => Some(x)) == None)
assert(some2.flatMap((x) => Some(x)) == Some(3))


//Tests for getOrElse
val none3: Option[Int] = None
val some3: Option[Int] = Some(33)
assert(none3.getOrElse(-1) == -1)
assert(some3.getOrElse(-1) == 33)

// Tests for orElse
val none4: Option[Int] = None
val some4: Option[Int] = Some(4)

val result4a = none4.orElse(Some(-1))
val result4b = some4.orElse(Some(-1))

//Tests for filter
val none5: Option[Int] = None
val some5: Option[Int] = Some(99)
val some5a: Option[Int] = Some(4)

val result5a = none5.filter((x) => x == 99)
val result5b = some5.filter((x) => x == 99)
val result5c = some5a.filter((x) => x == 99)

assert(result5a == None)
assert(result5b == Some(99))
assert(result5c == None)









