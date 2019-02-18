import hw.streams.Generator

object Main extends hw.streams.SolutionLike { // do not change this line

  // This helper function is very useful
  def cons[A](head: A, tail: =>Generator[A]): Generator[A] = new Generator[A] {
    def next() = (head, tail)
  }

  // The example from the project description
  val ones: Generator[Int] = cons(1, ones)

  def from(x: Int): Generator[Int] = cons(x, from(x+1))

  def map[A,B](f: A => B, agen: Generator[A]): Generator[B] = 
  {
    val (head, tail) = agen.next()
    cons(f(head), map(f, tail))
  }

  val pow: Generator[Int] = map((x: Int) => math.pow(2,x).toInt, from(0))


  def nth[A](agen: Generator[A], index: Int): A = 
  {
    val (head, next) = agen.next()
    if(index==0)
      head
    else
      nth(next, index-1)
  }


  def filter[A](pred: (A) => Boolean, agen: Generator[A]): Generator[A] = 
  {
    val (head, tail) = agen.next()
    if(pred(head)== true)
      cons(head, filter(pred, tail))
    else
      filter(pred, tail)
  }

  def interleave[A](agen1: Generator[A], agen2: Generator[A]): Generator[A] = 
  {
    val (head1, tail1) = agen1.next()
    val (head2, tail2) = agen2.next()
    cons(head1, cons(head2, interleave(tail1, tail2)))
  }

  def sift(n: Int, agen: Generator[Int]): Generator[Int] = 
  {
    val (head, tail) = agen.next()
    if(head%n == 0)
      sift(n, tail)
    else
      cons(head, sift(n, tail))
  }

  def sieve(agen: Generator[Int]): Generator[Int]=
  {
    val (head, tail) = agen.next()
    cons(head, sieve(sift(head, tail)))
  }
  
  val prime: Generator[Int] = sieve(from(2))

  def totalHelper(n: Double, agen: Generator[Double]): Generator[Double]=
  {
    val (head, tail) = agen.next()
    cons(n+head, totalHelper(n+head,tail))
  }
  def total(agen: Generator[Double]): Generator[Double] = totalHelper(0, agen)
}