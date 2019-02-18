import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def calcAllPos(ix: Int): List[(Int, Int)]= 
  {
    if(ix==81) 
     Nil
    else
      (ix/9, ix%9) :: calcAllPos(ix+1)
  }

  val allPos= calcAllPos(0)

  def parseHelper(alist: List[(Char, (Int, Int))]): Board= alist match
  {
    case Nil=> new Board(allPos.map(coord=> coord-> 1.to(9).toList).toMap)
    case ('.', _) :: rest => parseHelper(rest)
    case (digit, (row, col)) :: rest => 
    {
      val n= digit.toString.toInt
      parseHelper(rest).place(row,col,n)
    }
  }  
  def parse(str: String): Board = parseHelper(str.toList.zip(allPos))

  def calcPeers(row: Int, col: Int): List[(Int, Int)]= 
  {
    val rowPeers = 0.to(8).map(r=>(r,col))
    val colPeers = 0.to(8).map(c=>(row,c))
    val boxRow = (row/3)*3
    val boxCol = (col/3)*3
    val boxPeers = boxRow.to(boxRow+2).flatMap(r=>boxCol.to(boxCol+2).map(c=>(r,c)))
    (rowPeers ++ colPeers ++ boxPeers).toSet.diff(Set((row,col))).toList
  }

  val peersTb1 = allPos.map(pos=> 
  {
    val (row,col)= pos
    pos -> calcPeers(row,col)
  }).toMap

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = peersTb1((row,col))
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = 
  {
    val num= availableValuesAt(row,col)
    if(num.size ==1)
      Some(num.head)
    else
      None
  }

  def isSolved(): Boolean = available.forall(x=> x._2.size ==1)

  def isUnsolvable(): Boolean = !available.forall(x=> x._2.size !=0)

  def placeHelper(peers: List[(Int,Int)], value: Int, aboard: Board): Board = peers match
  {
    case Nil=> aboard
    case head::tail=>
    {
      val filteredBoard= aboard.available + (head-> aboard.availableValuesAt(head._1, head._2).filterNot(v=> v== value))
      if(filteredBoard.size == 1)
        placeHelper(tail, value, placeHelper(Solution.peers(head._1, head._2), filteredBoard(head).head, new Board(filteredBoard)))
      else
        placeHelper(tail, value, new Board(filteredBoard))
    }
  }
  def place(row: Int, col: Int, value: Int): Board = 
  {
    require(availableValuesAt(row, col).contains(value))
    placeHelper(Solution.peers(row,col), value, new Board(available + ((row,col)-> List(value))))
  }

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = 
  {
    if (isUnsolvable()) {
      List[Board]()
    }
    else 
    {
      available.foldLeft(List[Board]())((x,y) => 
        if(available(y._1).size == 1)
          x
        else
          x ++: available(y._1).foldLeft(List[Board]())((a,b) => a :+ this.place(y._1._1, y._1._2, b))).sortBy(m=> m.available.foldLeft(0)((a,b)=> a + m.available(b._1).size))
    }
  }
 

  def solveHelper(alist: List[Board]): Option[Board] = alist match
  {
    case head::tail =>
    {
      if(head.solve != None)
        head.solve
      else
        solveHelper(tail)
    }
    case _ => None
  } 

  def solve(): Option[Board] = 
  {
    if(isSolved())
      Some(this)
    else
      solveHelper(this.nextStates)
  }
}