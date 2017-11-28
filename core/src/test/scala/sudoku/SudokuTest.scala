package sudoku

import org.scalatest.matchers.Matcher
import org.scalatest.{FlatSpec, FunSuite, Matchers}

import scala.io.Source

/**
  * Created by Cesar on 05/07/2016.
  */
class SudokuTest extends FlatSpec with Matchers {

  case class SudokuExample(input: Sudoku, solution: Sudoku) {
    def result: Result = input.solve() match {
      case None => NoSolved
      case Some(i) if i == solution => Solved(i)
      case Some(i) => Wrong(input)
    }
  }

  sealed trait Result

  case object NoSolved extends Result

  case class Solved(sudoku: Sudoku) extends Result

  case class Wrong(sudoku: Sudoku) extends Result

  def readExample(fileName: String): Vector[SudokuExample] = {
    val in = getClass.getResourceAsStream(fileName)

    val entries =
      Source
        .fromInputStream(in).
        getLines()
        .toVector
        .drop(1)

    Sudoku.traverse(entries) { line =>
      val List(fst, snd) = line.split(",").toList
      for {
        input <- Sudoku.parse(fst)
        output <- Sudoku.parse(snd)
      } yield SudokuExample(input, output)
    }.fold(
      error => {
        sys.error("Could not read example: " + error)
      },
      identity
    )
  }

  /*
    Test cases generated using https://qqwing.com/instructions.html
   */
  val simpleExamples: Vector[SudokuExample] = readExample("/sudoku-simple.csv")
  val easyExamples: Vector[SudokuExample] = readExample("/sudoku-easy.csv")

  "A sudoku" should "solve simple cases" in {
    val start = System.currentTimeMillis()
    simpleExamples should not be empty
    simpleExamples.length shouldBe 100
    simpleExamples.foreach { case SudokuExample(input, output) =>
      input.solve() shouldEqual Some(output)
    }
    val stop = System.currentTimeMillis();
    println(stop - start)
  }

  "A sudoku" should "be fast" in {
    val start = System.currentTimeMillis()
    simpleExamples should not be empty
    simpleExamples.length shouldBe 100
    simpleExamples.foreach { case SudokuExample(input, output) =>
      input.solve()
    }
    val stop = System.currentTimeMillis();
    println("total time")
    println(stop - start)
  }

  "A sudoku input" should "be exactly 81 characters long" in {
    Sudoku.parse("abc") shouldBe Left(InvalidGridSize(3))
  }

  "A sudoku input" should "only should have characters matching /\\.|[1-9]/" in {
    Sudoku.parse(".93.2..6.4.21...h..1...8......4...2....76.14......2..9..6...7.3..9.87..2.......9.") shouldBe Left(InvalidCell('h'))
  }
}
