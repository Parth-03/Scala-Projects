object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match
{
	case (Nil,Nil)=> Nil
	case (head1::tail1, Nil) => Nil
	case (Nil, head2::tail2) => Nil
	case (head1::tail1, head2::tail2) => f(head1,head2):: map2(f,tail1,tail2)
}

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1,lst2) match
{
	case (Nil,Nil)=>Nil
	case (head1::tail1, Nil) => Nil
	case (Nil, head2::tail2) => Nil
	case (head1::tail1, head2::tail2)=> (head1,head2)::zip(tail1,tail2)
}

  def concat[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match
{
	case Nil=> lst2
	case head::tail=> head :: concat(tail,lst2)
}
  def flatten[A](lst: List[List[A]]): List[A] = lst match
{
	case Nil=> Nil
	case head::tail=> concat(head, flatten(tail))
}

  def flatten3[A](lst: List[List[List[A]]]): List[A] =  lst match
{
	case Nil=> Nil	
	case head::tail => concat(flatten(head), flatten3(tail))
}

  def buildhelper[A](x: Int, length: Int, f: Int=> A) : List[A]=
{
	if(x>=length)
		Nil
	else
		f(x):: buildhelper(x+1, length,f)
}
  def buildList[A](length: Int, f: Int => A): List[A] = 
{
	buildhelper(0,length,f)	
}

  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match
{
	case Nil=>Nil
	case head::tail=> concat(f(head), mapList(tail, f))
}

  def reversal[A](alist: List[A]): List[A] = 
{
	def rlRec[A](result: List[A], alist:List[A]): List[A] = alist match
	{
		case Nil=> result
		case head::tail=> rlRec(head::result, tail)
	}
	rlRec(Nil, alist)
}

  def partitionhelper[A](f:A=> Boolean, lst: List[A], left : List[A], right : List[A]): (List[A],List[A])= lst match
{
	case Nil=> (reversal(left),reversal(right))
	case head::tail=>
	{
		if(f(head))
			partitionhelper(f, tail, head::left,right)
		else
			partitionhelper(f,tail,left,head::right)
	}
}
	
  def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = 
{
	partitionhelper(f, lst, List(), List()) 
					
}
  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = (alist1,alist2) match
{
	case (Nil,Nil) => Nil
	case (alist1, Nil)=> alist1
	case (Nil, alist2) => alist2
	case (head1::tail1, head2::tail2) =>
	{
		if(lessThan(head1,head2))
			head1::merge(lessThan, tail1, alist2)
		else
			head2::merge(lessThan, alist1, tail2)
	} 
}

  def sortHelper[A](lessThan: (A,A) => Boolean, x:A, alist: List[A]): List[A]= alist match
{
	case Nil=> List(x)
	case head::tail => 
	{
		if(lessThan(x,head))
			x::head::tail
		else
			head:: sortHelper(lessThan, x, tail)
	}
}

  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match
{
	case Nil=> Nil
	case head:: tail=> sortHelper(lessThan, head, sort(lessThan, tail))
}
}