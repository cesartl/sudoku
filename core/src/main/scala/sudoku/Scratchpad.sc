trait Shape

case class Circle(diameter: Double) extends Shape

case class Rectangle(width: Double, height: Double) extends Shape

val circle = Circle(22)
val rectangle = Rectangle(10, 20)

def shapePrint(shape: Shape): String = shape match {
  case Circle(diameter) => s"Circle with diameter $diameter"
  case Rectangle(w, h) => s"Rectangle [w= $w, h=$h]"
}

shapePrint(circle)
shapePrint(rectangle)


val l: List[Int]

def map[A, B](as: List[A])(f: A => B): List[B] = ???

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = ???

val l1 = List(1, 2, 3)
l1.map(x => 2 * x) //= List(2, 4, 6)

l1.filter(x => x % 2 == 0) //= List(2)

l1.flatMap(x => List(x, x))


val row1 = Vector(1, 2, 3, 4): Vector[Int]
val row2 = Vector(5, 6, 7, 8): Vector[Int]
val row3 = Vector(9, 10, 11, 12): Vector[Int]
val row4 = Vector(13, 14, 15, 16): Vector[Int]

val matrix = Vector(row1, row2, row3, row4) : Vector[Vector[Int]]

val g = matrix.grouped(2).toList

var colIndex = 0
var rowIndex = 1

g(rowIndex)(1).grouped(2).toList(0)


matrix.grouped(2)


//println(g(0))
/*
  g = [
    [[1, 2, 3, 4],[5, 6, 7, 8]],
    [[9, 10, 11, 12], [9, 10, 11, 12]]
  ]
*/



val square = g(rowIndex)
  .flatMap(row => row.grouped(2).toList(colIndex))



/*
  square = [3, 4, 7, 8]
 */


val list = List("a", "b", "c")
val zipped = list.zipWithIndex
//zipped = List((a,0), (b,1), (c,2))


val tuple = (1, 2, 3)
//val tuple = Tuple3(1, 2, 3)
val e1 = tuple._1 //=1
val e2 = tuple._2 //=2
val e3 = tuple._3 //=3


matrix.zipWithIndex







