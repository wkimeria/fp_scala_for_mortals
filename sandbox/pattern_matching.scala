sealed trait Child
case class Good(name: String, age: Int) extends Child
case class Bad(name: String, age: Int) extends Child


val goodChild: Child = Bad("Goody Two shoes", 2)

/*
Santa's Code to check whether to give a gift. However if a child is
one or younger, treat them like a good child
child 
*/

val greeting = goodChild match {
	case Bad(name, 1) => "Hello " + name + "!. Here is a gift."
	//case Bad(name, _) =>  "Hello " + name + "!. Here is a lump of coal." //Comment out this line
	case Good(name, _) => "Hello " + name + "!. Here is a gift."
}

println(greeting)
