package sudoku

import scala.annotation.tailrec

sealed trait Cell extends Product with Serializable {
  def printCell(): String = {
    this match {
      case Fixed(e) => e.toString
      case Undetermined(values) => "0"
    }
  }
}

case class Fixed(e: Int) extends Cell

case class Undetermined(values: List[Int]) extends Cell

case class Sudoku(grid: Vector[Vector[Cell]]) {

  /**
    * Returns true if the given value, at the given position, is allowed within its row.
    * Logically this is true if the row doesn't already contain a Fixed cell with the given value
    *
    * @param position The position of the cell being checked
    * @param value    The value to test
    * @return true if the value is allowed in that cell
    */
  def isValueAllowedInRow(position: (Int, Int), value: Int): Boolean = {
    !grid(position._1).contains(Fixed(value))
  }

  /**
    * Returns true if the given value, at the given position, is allowed within its column.
    * Logically this is true if the column doesn't already contain a Fixed cell with the given value
    *
    * @param position The position of the cell being checked
    * @param value    The value to test
    * @return true if the value is allowed in that cell
    */
  def isValueAllowedInColumn(position: (Int, Int), value: Int): Boolean = {
    val t = grid.transpose
    !t(position._2).contains(Fixed(value))
  }

  /**
    * Returns true if the given value, at the given position, is allowed within its square.
    * Logically this is true if the square doesn't already contain a Fixed cell with the given value
    *
    * @param position The position of the cell being checked
    * @param value    The value to test
    * @return true if the value is allowed in that cell
    */
  def isValueAllowedInSquare(position: (Int, Int), value: Int): Boolean = {
    val ci = position._1 / 3
    val cj = position._2 / 3
    val squareCells = grid.grouped(3).toList(ci).flatMap { x => x.grouped(3).toList(cj) }
    !squareCells.contains(Fixed(value))
  }

  /**
    * Returns true of the given value, at the given position is allowed. This methods combine the row, column and
    * square strategies
    *
    * @param position The position of the cell being checked
    * @param value    The value to test
    * @return true if the value is allowed in that cell
    */
  def isValueAllowed(position: (Int, Int), value: Int): Boolean =
    isValueAllowedInRow(position, value) &&
      isValueAllowedInColumn(position, value) &&
      isValueAllowedInSquare(position, value)

  @tailrec
  final def filter(): Sudoku = {
    println("filtering")
    val sn1: Vector[Vector[Cell]] = grid
      .zipWithIndex
      .map { row: (Vector[Cell], Int) =>
        row._1
          .zipWithIndex
          .map {
            case (Undetermined(values), colIdx) =>
              Undetermined(values
                .filter(x => isValueAllowed((row._2, colIdx), x))
              )
            case (x, _) => x
          }
          .map {
            case Undetermined(values) if values.size == 1 => Fixed(values.head)
            case x => x
          }
      }
    if (sn1 == grid) this
    else Sudoku(sn1).filter()
  }

  def serialise(): String =
    grid.flatMap(row => row).map {
      case Fixed(e) => e.toString
      case Undetermined(values) => "."
    }.mkString("")

  def printGrid(): String =
    grid.map(
      _.map(c => c.printCell()).mkString("")
    ).mkString("\n")
}

object Sudoku {

  def parse(s: String): Vector[Vector[Cell]] = {
    s
      .toVector
      .map {
        case '.' => Undetermined((1 to 9).toList)
        case w => Fixed(w.toString.toInt)
      }
      .grouped(9)
      .toVector
  }


  def isSolved(grid: Vector[Vector[Cell]]): Boolean = {
    grid.flatten.forall(c => c.isInstanceOf[Fixed])
  }

  def main(args: Array[String]): Unit = {
    val gridString = ".4.87.1..1....3.59..8.9.47.25..19....37...91....58..34.26.3.5..49.7....2..5.41.9."
    val grid = Sudoku(parse(gridString))
    val solved = grid.filter()
    println(solved.serialise())
    println(solved.printGrid())
  }
}