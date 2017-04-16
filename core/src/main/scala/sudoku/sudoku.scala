package sudoku

import scala.annotation.tailrec
import scala.util.Try

sealed trait Cell extends Product with Serializable {
  def printCell(): String = {
    this match {
      case Fixed(e) => e.toString
      case Undetermined(values) => "0"
    }
  }

  def isFixed: Boolean = fold(_ => true, _ => false)

  def fold[A](f: Int => A, g: List[Int] => A): A = this match {
    case Fixed(i) => f(i)
    case Undetermined(is) => g(is)
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


  def isSolved: Boolean =
    grid.flatten.forall(_.isFixed)

  @tailrec
  final def solve(): Option[Sudoku] = {
    val filtered: Vector[Vector[Cell]] = grid
      .zipWithIndex
      .map { case (row, rowIdx) =>
        row
          .zipWithIndex
          .map {
            case (Undetermined(values), colIdx) =>
              Undetermined(values
                .filter(x => isValueAllowed((rowIdx, colIdx), x))
              )
            case (x, _) => x
          }
          .map {
            case Undetermined(values) if values.size == 1 => Fixed(values.head)
            case x => x
          }
      }
    if (filtered == grid) {
      if (isSolved) Some(this) else None
    }
    else Sudoku(filtered).solve()
  }

  def serialise(): String =
    grid.flatten.map {
      case Fixed(e) => e.toString
      case Undetermined(_) => "."
    }.mkString

  def printGrid(): String =
    grid.map(
      _.map(c => c.printCell()).mkString
    ).mkString("\n")
}

object Sudoku {

  def validateSize(s: String): Option[Unit] = if (s.length == 9 * 9) Some(()) else None

  def parseCell(s: Char): Option[Cell] = s match {
    case '.' => Some(Undetermined((1 to 9).toList))
    case w => Try(w.toString.toInt).toOption.map(Fixed)
  }

  def parse(s: String): Option[Sudoku] = {
    for {
      _ <- validateSize(s)
      grid <- traverse(s.toVector)(parseCell)
    } yield Sudoku(grid.grouped(9).toVector)
  }

  def traverse[A, B](xs: Vector[A])(f: A => Option[B]): Option[Vector[B]] =
    xs.foldRight[Option[Vector[B]]](Some(Vector())) { (a, buff) =>
      buff.flatMap(list => f(a).map(b => b +: list))
    }


  def main(args: Array[String]): Unit = {
    val gridString = ".4.87.1..1....3.59..8.9.47.25..19....37...91....58..34.26.3.5..49.7....2..5.41.9."
    val grid = parse(gridString).getOrElse(sys.error("Could not parse"))
    val solved = grid.solve()
    println(solved.map(_.serialise()))
    println(solved.map(_.printGrid()))
  }
}
