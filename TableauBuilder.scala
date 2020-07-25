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
        var nvarScarto = 0
        for (i <- 1 to segni.length){
            segni(i-1) match {
                case "<=" => {
                    A(i-1)(nvar + nvarScarto + 1) = 1
                    nvarScarto += 1
                }
                case ">=" => {
                    A(i-1)(nvar + nvarScarto + 1) = -1; 
                    nvarScarto += 1
                }
                case _ => 
            }
        }
        A = A.slice(1, nvar + nvarScarto + 1) 

        //Se esiste b_i negativo, lo rendo positivo moltiplicando per -1 A_i e b_i
        for(i <- 0 until A.length)
            if(b(i) < 0){
                A.mat(i) = A(i).map(_ * -1) 
                b(i) *= -1
            }
        
        //se l'ultima parte della matrice è uguale alla matrice identità, allora usiamo quella come base iniziale
        if (A.slice(A(0).length - A.length, A(0).length).equals(MatrixTools.identity(A.length))) { 
            val inBase = List.range(A(0).length - A.length, A(0).length).to[ListBuffer]
            val fuoriBase = List.range(0, A.nCols).filter(n => !inBase.contains(n)).to[ListBuffer]

            val B = A.getCols(inBase.toList).T
            val ccr = new Matrix(List(c.slice(1, nvar + nvarScarto + 1)))//.mult(B.inv)

            val terminiNoti = new Matrix(List(b.toList)).mult(B.inv).T
            val tableau = new Tableau(min, false, ccr, ccr, A, inBase, fuoriBase, terminiNoti)
            tableau
        }
        else {
            val dueFasiA = A.concat(MatrixTools.identity(A.length))

            val ccr = new Matrix(List(c.slice(1, nvar + nvarScarto + 1)))//.mult(B.inv)

            val dueFasiCcr = new Matrix(List(List.tabulate(dueFasiA(0).length)(n => if (n < A(0).length) 0 else 1)))

            val dueFasiterminiNoti = new Matrix(List(b.toList)).T
            val dueFasiFuoriBase = ListBuffer.range(0, A(0).length)
            val dueFasiInBase = ListBuffer.range(A(0).length, dueFasiA(0).length)
            
            val dueFasiTableau = new Tableau(min, true, dueFasiCcr, ccr, dueFasiA, dueFasiInBase, dueFasiFuoriBase, dueFasiterminiNoti)
            dueFasiTableau.canonicizza_ccr
            dueFasiTableau
        }

    }

}


