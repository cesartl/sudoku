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

  def readExample(fileName: String): List[SudokuExample] = {
    val in = getClass.getResourceAsStream(fileName)
    Source
      .fromInputStream(in).
      getLines()
      .toList
      .drop(1)
      .map { line =>
        val List(fst, snd) = line.split(",").toList
        for {
          input <- Sudoku.parse(fst)
          output <- Sudoku.parse(snd)
        } yield SudokuExample(input, output)
      }.collect { case Some(e) => e }
  }

  /*
    Test cases generated using https://qqwing.com/instructions.html
   */
  val simpleExamples: List[SudokuExample] = readExample("/sudoku-simple.csv")
  val easyExamples: List[SudokuExample] = readExample("/sudoku-easy.csv")

  "A sudoku" should "solve simple cases" in {
    val start = System.currentTimeMillis();
    simpleExamples.foreach { case SudokuExample(input, output) =>
      input.solve() shouldEqual Some(output)
    }
    val stop = System.currentTimeMillis();
    println(stop - start)
  }

}
