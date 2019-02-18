import  hw.csv._

sealed trait Gender
case class Male() extends Gender
case class Female() extends Gender

case class SSARow(birthYear: Int, name: String, gender: Gender, count: Int)

case class CDCRow(birthYear: Int, maleLifeExpectancy: Int,
  femaleLifeExpectancy: Int)

object Main {

  def readSSARow(row: List[String]): SSARow = 
{
	SSARow(row(0).toInt, row(1), if(row(2) =="F") Female() else Male(), row(3).toInt)
}

  def readCDCRow(row: List[String]): CDCRow = 
{
	CDCRow(row(0).toInt, row(1).toInt, row(2).toInt)
}

  def yearIs(rows: List[SSARow], bound: Int): List[SSARow] = 
{
	rows.filter(row=> row.birthYear== bound)
}

  def yearGT(rows: List[SSARow], bound: Int): List[SSARow] = 
{
	rows.filter(row=> row.birthYear > bound)
}

  def yearLT(rows: List[SSARow], bound: Int): List[SSARow] = 
{
	rows.filter(row=> row.birthYear < bound)
}

  def onlyName(rows: List[SSARow], name: String): List[SSARow] = 
{
	rows.filter(row=> row.name == name)
}

//def f(x: Int, y: Int): Boolean = x>y

def helper(rows: List[(String, Int)], commonName: String, highestCount: Int): (String, Int)= rows match
{
	case Nil=> (commonName, highestCount)
	case head::tail=>
	{
		if(head._2>highestCount)
			helper(tail, head._1, head._2)
		else
			helper(tail, commonName, highestCount)
	}
}

def mapHelper(rows: List[(String, List[SSARow])]): List[(String, Int)]= rows match
{
	case Nil=> Nil
	case head::tail=> (head._1, count(head._2)) :: mapHelper(tail)
}

  def mostPopular(rows: List[SSARow]): (String, Int) = 
{
	helper(mapHelper(rows.groupBy(row=> row.name).toList), "", 0) 
}	

  def count(rows: List[SSARow]): Int = rows match
{
	case Nil => 0
	case head:: Nil=> head.count
	case head:: tail=> head.count + count(tail)
}

  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = 
{
	(count(rows.filter(row=> row.gender == Female())), count(rows.filter(row=> row.gender == Male())))
}

def getName(rows: List[SSARow]): List[String]= rows match
{
	case Nil=> Nil
	case head:: tail=> head.name:: getName(tail)
}

  def genderNeutralNames(rows: List[SSARow]): Set[String] = 
{
	getName(rows.filter(row=> row.gender == Female())).toSet.intersect(getName(rows.filter(row=> row.gender == Male())).toSet)
}

  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = 
{
	if(gender== Female())
	{
		if(lifeExpectancies.filter(row=> row.birthYear == birthYear)(0).femaleLifeExpectancy + birthYear >= currentYear)
			true
		else
			false
	}
	else
	{
		if(lifeExpectancies.filter(row=> row.birthYear == birthYear)(0).maleLifeExpectancy + birthYear >= currentYear)
			true
		else
			false
	}
}

  def estimatePopulation(rows: List[SSARow], year: Int,
    lifeExpectancies: List[CDCRow]): Int = rows match
{
	case Nil=> 0
	case head:: tail=> 
	{
		if(expectedAlive(head.gender, head.birthYear, year, lifeExpectancies))
		{
			head.count + estimatePopulation(tail, year, lifeExpectancies)
		}
		else
			estimatePopulation(tail, year, lifeExpectancies)
	}
}

}