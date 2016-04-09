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

var s = sudoku
.split("\n")
.slice(1,10)
.map(row=>row
    .split("")
    .slice(1,10)
    .map(cell=>cell match {
        case " " => (1 to 9).toArray
        case _ => Array(cell.toInt)
    }
    )
)

def uniques(s: Array[Array[Array[Int]]]): Array[Array[Int]] = {
    s
    .map{row=>row
        .map{cell=>
            if (cell.size != 1){ 0 }
            else { cell(0) }
        }
    }
}

def myNeighbourRow(s: Array[Array[Array[Int]]], p:(Int,Int), e:Int) = {
    uniques(s)(p._1).count(_==e)<1 || uniques(s)(p._1)(p._2) == e
}

def myNeighbourColumn(s: Array[Array[Array[Int]]], p:(Int,Int), e:Int) = {
    val t = uniques(s).transpose
    t(p._2).count(_==e)<1 || uniques(s)(p._1)(p._2) == e
}

def myNeighbourCube(s: Array[Array[Array[Int]]], p:(Int,Int), e:Int) = {
    var ci = p._1 / 3
    var cj = p._2 / 3
    val t = uniques(s).grouped(3).toList(ci).map{x=>(x.grouped(3).toList)(cj)}.flatten
    //println(t.toList)
    uniques(s)(p._1).count(_==e)<1 || uniques(s)(p._1)(p._2) == e
}

def filter(sn: Array[Array[Array[Int]]]):Array[Array[Array[Int]]] = {
    var sn1 = sn
    .zipWithIndex
    .map{row=>row._1
        .zipWithIndex
        .map{cell=>cell._1
            .filter(myNeighbourRow(sn,(row._2,cell._2),_))
            .filter(myNeighbourColumn(sn,(row._2,cell._2),_))
            .filter(myNeighbourCube(sn,(row._2,cell._2),_))
        }
    }
    if(sn1.deep==sn.deep) sn
    else filter(sn1)
    //sn1
    
}

var solved = filter(s)
solved.map{row=>row
    .map{_(0)}
    .mkString("")
}
.mkString("\n")
