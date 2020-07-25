import scala.collection.mutable.ListBuffer

class TableauBuilder {
    var c : List[Double] = List()
    var b = new ListBuffer[Double]()
    var A = new Matrix(0,0)
    var segni = new ListBuffer[String]()
    var min = true


    def nVar = {
        var n = 0
        for (j <- 0 until A(0).length) {
            for (i <- 0 until A.length) {
                if(A(i)(j) != 0)
                    n = j
            }
        } 
        n
    }

    def build = {
        val nvar = nVar
        for (i <- 1 to segni.length){
            segni(i-1) match {
                case "<=" => A(i-1)(nvar + i) = 1
                case "=" => A(i-1)(nvar + i) = 0
                case ">=" => A(i-1)(nvar + i) = -1
            }
        }
        A = A.slice(1, nvar + segni.length + 1) 

        //Se esiste b_i negativo, lo rendo positivo moltiplicando per -1 A_i e b_i
        for(i <- 0 until A.length)
            if(b(i) < 0){
                A.mat(i) = A(i).map(_ * -1) 
                b(i) *= -1
            }

        val inBase = trovaBase.to[ListBuffer]
        val fuoriBase = List.range(0, A.nCols).filter(n => !inBase.contains(n)).to[ListBuffer]

        val B = A.getCols(inBase.toList).T
        val ccr = new Matrix(List(c.slice(1, nvar + segni.length + 1)))//.mult(B.inv)

        val terminiNoti = new Matrix(List(b.toList)).mult(B.inv).T
        val tableau = new Tableau(min, ccr, A, inBase, fuoriBase, terminiNoti)
        tableau
    }

    def trovaBase = {
        var varBase : ListBuffer[Int] = ListBuffer()
        //se l'ultima parte della matrice è uguale alla matrice identità, allora usiamo quella come base iniziale
        if (A.slice(A(0).length - A.length, A(0).length).equals(MatrixTools.identity(A.length))) { 
            List.range(A(0).length - A.length, A(0).length)
        }
        else {//TODO: due fasi
            throw new Exception("TODO: due fasi")
            List()
        }
    }

}


