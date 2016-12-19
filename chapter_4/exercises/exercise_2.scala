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
Implement the variance function in terms of flatMap. 
If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence. 
See the definition of variance on Wikipedia 
(http://mng.bz/0Qsr).

def variance(xs: Seq[Double]): Option[Double]

*/


def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))



val values:Seq[Double] = Seq(1.0, 2.0, 3.0, 4.0, 6.0);

//val variance = values.fold(0.0)((acc,v) => acc + math.pow(v - mean, 2));

println(mean(values));

val xs = values;
xs.fold(0.0)((acc,v) => acc + v)
println(xs.length);