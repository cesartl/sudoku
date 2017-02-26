sealed trait Cell extends Product with Serializable

case class Fixed(e:Int) extends Cell

case class Undetermined(list:List[Int]) extends Cell

val x = 1

val sudoku = """
   26 7 1
68  7  9
19   45
82 1   4
  46 29
 5   3 28
  93   74
 4  5  36
7 3 18
             """

val s = sudoku
  .split("\n")
  .slice(1,10)
  .map(row=>row
    .split("")
    .slice(1,10)
    .map(cell=>cell match {
      case " " => Undetermined((1 to 9).toList)
      case _ => Fixed(cell.toInt)
    }
    ).toVector
  ).toVector


def myNeighbourRow(s: Vector[Vector[Cell]], p:(Int,Int), e:Int) = {
  (!s(p._1).contains(Fixed(e))) || s(p._1)(p._2) == Fixed(e)
}

def myNeighbourColumn(s: Vector[Vector[Cell]], p:(Int,Int), e:Int) = {
  val t = s.transpose
  (!t(p._2).contains(Fixed(e))) || s(p._1)(p._2) == Fixed(e)
}

def myNeighbourCube(s: Vector[Vector[Cell]], p:(Int,Int), e:Int) = {
  val ci = p._1 / 3
  val cj = p._2 / 3
  val t = s.grouped(3).toList(ci).map{x=>(x.grouped(3).toList)(cj)}.flatten
  (!t.contains(Fixed(e))) || s(p._1)(p._2) == Fixed(e)
}

def filter(sn: Vector[Vector[Cell]]):Vector[Vector[Cell]] = {
  val sn1:Vector[Vector[Cell]] = sn
    .zipWithIndex
    .map{row:(Vector[Cell],Int)=>row._1
      .zipWithIndex
      .map{
        case (Undetermined(values),index_x)=>Undetermined(values
          .filter(x=>myNeighbourRow(sn,(row._2,index_x),x))
          .filter(x=>myNeighbourColumn(sn,(row._2,index_x),x))
          .filter(x=>myNeighbourCube(sn,(row._2,index_x),x)))
        case (x,_)=>x//_._1 // (Fixed(value),index_x)=>value//Vector(Fixed(value))
      }
      .map{
        case Undetermined(values) if (values.size==1)=>Fixed(values(0))
        case x => x
      }
    }
  if(sn1==sn) sn
  else filter(sn1)
  //sn1

}

var solved = filter(s)

var printable = solved.map{_.map(_.asInstanceOf[Fixed].e).mkString("")}.mkString("\n")

println(printable)


