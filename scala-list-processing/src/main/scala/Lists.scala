object Lists {

 	val oddNumbers= 1 :: 3 :: 5 :: Nil

def sumDouble(alist : List[Int]): Int =alist match
{
	case Nil=>0
	case head::tail=> (2*head) + sumDouble(tail)
}

def removeZeroes(alist : List[Int]): List[Int] =alist match
{
	case Nil=>Nil
	case head::tail=>
		if(head==0)
			removeZeroes(tail)
		else
			head::removeZeroes(tail)
}		

def countEvens(alist : List[Int]): Int = alist match
{
	 case Nil=>0
	 case head::tail=>
	 	if(head%2==0)
	 		countEvens(tail)+1
	 	else
	 		countEvens(tail)
}

def removeAlternating(alist: List[String]): List[String] =alist match
{
	case Nil=>Nil
	case head::Nil=> head::Nil
	case head::x::tail=> head:: removeAlternating(tail)
}

def isAscending(alist : List[Int]): Boolean= alist match
{
	case Nil=> true
	case head::Nil=> true
	case head::tail=> head<=tail.head && isAscending(tail)
}

def addSubHelper(x: Int, alist : List[Int]): Int= alist match
{
	case Nil=> 0
	case head::tail=>
		if(x%2==0)
			head+addSubHelper(x+1,tail)
		else
			(-head)+addSubHelper(x+1,tail)
}	

def addSub(alist : List[Int]): Int= alist match
{
	case Nil=>0
	case head::tail=> addSubHelper(0,alist)
}

def alternate(alist :List[Int], blist : List[Int]): List[Int] = (alist,blist) match
{
	case (Nil,Nil)=> Nil
	case(head::tail,Nil)=> alist
	case(Nil,head::tail)=> blist
	case(head::tail, head1::tail1)=> head::head1::alternate(tail,tail1)
}

def fromTo(x: Int, y: Int): List[Int]= 
{
	if(x==y)
		Nil
	else
		x::fromTo(x+1,y)
}

def insertOrdered(n: Int, lst: List[Int]): List[Int]= lst match
{
	case Nil=> n::Nil
	case head::tail=>
		if(n<=head)
			n::head::tail
		else
			head:: insertOrdered(n,tail)
}

def sortLT(x: Int, alist: List[Int]): List[Int]=alist match
{
	case Nil=> Nil
	case head::tail=>
		if(head<x)
			head::sortLT(x,tail)
		else
			sortLT(x,tail)
}

def sortGT(x: Int, alist: List[Int]): List[Int]=alist match
{
	case Nil=> Nil
	case head::tail=>
		if(head>=x)
			head::sortGT(x,tail)
		else
			sortGT(x,tail)
}

def sort(alist: List[Int]): List[Int] =alist match
{
	case Nil=> Nil
	case pivot::tail=> sort(sortLT(pivot,tail)) ::: List(pivot) ::: sort(sortGT(pivot,tail))
}

}