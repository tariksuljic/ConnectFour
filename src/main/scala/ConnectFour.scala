class ConnectFour {
  val rows = 6
  val cols = 7

  val board = Array.fill(rows, cols)(".")


  def play(col: Int, player: String): Unit = {
    if (board(0)(col) != ".") {
      println("Column is full, try again.")
      printBoard();
    } else {
      for (i <- (0 until rows).reverse) {
        if (board(i)(col) == ".") {
          board(i)(col) = player
          printBoard()
          return
          
        }
      }
    }
  }




  def playHuman(): Unit = {
    println("Player X or O?")
    var player = scala.io.StdIn.readLine().toUpperCase()
    while (player != "X" && player != "O") {
      println("Invalid input, please enter X or O.")
      player = scala.io.StdIn.readLine().toUpperCase()
    }
    var currentPlayer = player
    printBoard()

    while (true) {
      println(
        s"Player $currentPlayer turn. Which column do you want to play? (1-${cols})"
      )
      val col = scala.io.StdIn.readInt()
      if (col < 1 || col > cols) {
        println("Invalid column, try again.")
      } else {
        play(col-1, currentPlayer)
        if (checkForWinner()) {
          println(s"Player $currentPlayer wins!")
          return
        }
        if (isBoardFull()) {
          println("It's a draw!")
          return
        }
        currentPlayer = if (currentPlayer == "X") "O" else "X"
      }
    }
  }

  def checkForWinner(): Boolean = {
    // check horizontal
    for (i <- 0 until rows) {
      for (j <- 0 until cols - 3) {
        if (board(i)(j) != "." && board(i)(j) == board(i)(j + 1) && board(i)(j) == board(i)(j + 2) && board(i)(j) == board(i)(j + 3)
        )
          return true
      }
    }
    // check vertical
    for (i <- 0 until rows - 3) {
      for (j <- 0 until cols) {
        if (
          board(i)(j) != "." && board(i)(j) == board(i + 1)(j) && board(i)(
            j
          ) == board(i + 2)(j) && board(i)(j) == board(i + 3)(j)
        )
          return true
      }
    }
    // check diagonal
    for (i <- 0 to rows-4) {
        for (j <- 0 to cols-4) {
            if (board(i)(j) != "." && board(i)(j) == board(i+1)(j+1) && board(i+1)(j+1) == board(i+2)(j+2) && board(i+3)(j+3) == board(i+3)(j+3)) {
                return true
            }
        }
    }
    // check diagonal
    for (i <- 0 to rows-4) {
        for (j <- 3 until cols) {
            if (board(i)(j) != "." && board(i)(j) == board(i+1)(j-1) && board(i+1)(j-1) == board(i+2)(j-2) && board(i+3)(j-3) == board(i+3)(j-3)) {
                return true
            }
        }
    }
    return false
  }

 



  def printBoard(): Unit = {
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        print(board(i)(j) + " ")
      }
      println()
    }
  }



  def isBoardFull(): Boolean = {
    for (row <- board; cell <- row) {
      if (cell == ".") return false
    }
    return true
  }

}
