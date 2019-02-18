import hw.tictactoe._

class Game(/* add fields here */ turn: Player, dim: Int, board: Map[(Int,Int), Player] ) extends GameLike[Game] 
{

def getTurn: Player = turn

def getDim: Int = dim

def getBoard: Map[(Int, Int), Player] = board

  def isFinished(): Boolean = 
{
	if(getWinner == None)
	{
		if(board.size == dim*dim)
			true
		else
			false
	}
	else
		true
}
  /* Assume that isFinished is true */
  

def getWinner(): Option[Player] = 
{
		if(getWinnerHelper(X, dim - 1, board)){
			Some(X)
		}
		else if(getWinnerHelper(O, dim -1, board)){
			Some(O)
		}
		else None
	}

	def getWinnerHelper(name: Player, n: Int, board: Map[(Int, Int), Player]): Boolean= 
	{
		if(getRowWinner(name, n, board))
			true
		else if(getColWinner(name, n, board))
			true
		else if(getDiagonalWinner(name, n, board))
			true
		else if(getAntiDiagonalWinner(name, n, board))
			true
		else
			false
	}

	def getRowWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean =  row match 
	{
		case 0 => (board.filterKeys(x => x._2 == row).filter({ case (m, n) => (n == name)}).size == dim)
		case _ => {
			if(board.filterKeys(x => x._2 == row).filter({ case (m, n) => (n == name)}).size == dim) 
				true
			else getRowWinner(name, row - 1, board)
		}
	}

	def getColWinner(name: Player, col: Int, board: Map[(Int, Int), Player]): Boolean = col match 
	{
		case 0 => (board.filterKeys(x => x._1 == col).filter({ case (m, n) => (n == name)}).size == dim)
		case _ => {
			if(board.filterKeys(x => x._1 == col).filter({ case (m, n) => (n == name)}).size == dim) 
				true
			else getColWinner(name, col - 1, board)
		} 
	}

  	def getDiagonalWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean = 
	{
  		if(board.filterKeys(x => x._2 == x._1).filter({ case (m, n) => (n == name) }).size == dim) 
			true
  		else false
  	}
 

  	def getAntiDiagonalWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean = 
	{
  		if(board.filterKeys(x => x._2 + x._1 == dim - 1).filter({ case (m, n) => (n == name) }).size == dim)
			true
  		else false
  	}
	







  def nextBoards(): List[Game] = 
{
	if(isFinished()){
			Nil
		}
		else{
			nextBoardsHelper(dim - 1, dim - 1, board)
		}
	}

	def nextBoardsHelper(col: Int, row: Int, board: Map[(Int, Int), Player]): List[Game] = (col, row) match 
	{
		case (0, 0) => {
			if(!board.contains(0, 0)){
				if (getTurn == X) new Game(O, getDim, getBoard.+((0, 0) -> X)) :: Nil
				else new Game(X, getDim, getBoard.+((0, 0) -> O)) :: Nil
			}
			else Nil
		}
		case (x, 0) => {
			if(!board.contains(col, 0)){
				if(getTurn == X) new Game(O, getDim, getBoard.+((col, 0) -> X)) :: nextBoardsHelper(col - 1, dim - 1, board)
				else new Game(X, getDim, getBoard.+((col, 0) -> O)) :: nextBoardsHelper(col - 1, dim - 1, board)
			}
			else nextBoardsHelper(col - 1, dim - 1, board)
		}
		case (x, y) => {
			if(!board.contains(col, row)){
				if(getTurn == X) new Game(O, getDim, getBoard.+((col, row) -> X)) :: nextBoardsHelper(col, row - 1, board)
				else new Game(X, getDim, getBoard.+((col, row) -> O)) :: nextBoardsHelper(col, row - 1, board)
			}
			else nextBoardsHelper(col, row - 1, board)
		}
	}
}







object Solution extends MinimaxLike 
{

  type T = Game // T is an "abstract type member" of MinimaxLike
  
def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = 
{
	new Game(turn,dim,board)
}


  def minimax(board: Game): Option[Player] = 
{
	if(board.isFinished())
		board.getWinner()
	else
	{
		val nexts= board.nextBoards().toStream.map(minimax)
		if(board.getTurn==X)
		{
			if(nexts.contains(Some(X)))
				Some(X)
			else if(nexts.contains(None))
				None
			else 
				Some(O)
		}
		else
		{
			if(nexts.contains(Some(O)))
				Some(O)
			else if(nexts.contains(None))
				None
			else 
				Some(X)
		}
	}
			
}
	
	

}
