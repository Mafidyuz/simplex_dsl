import sys.process._
import java.io._
import scala.io.Source
import scala.collection.mutable.ListBuffer

class Matrix(mc: List[List[Double]]) {
    
    def this(n: Int, m: Int) = this(List.tabulate(n)(_ => List.tabulate(m)(_ => 0.0)))

    var mat = mc.map(l => l.to[ListBuffer]).to[ListBuffer]

    def apply(n: Int) = mat(n) 

    def T = new Matrix(mat.transpose.map(l => l.toList).toList)

    def printMatrix = {
        for (row <- mat){
            for (el <- row) 
                print("%.2f".format(el) + "\t")
            println()
        }
    }

    def addRow(row : ListBuffer[Double]) = {
        mat += row 
    }

    def slice(start: Int, end: Int) = {
        new Matrix(mat.map(l => l.slice(start,end).toList).toList)
    }

    def equals(M: Matrix): Boolean = {
        if (mat.length != M.length || mat.head.length != M(0).length) 
            return false
        else{
            for (i <- 0 until mat.length){
                for (j <- 0 until mat.head.length){
                    if (mat(i)(j) != M(i)(j))
                        return false
                }
            }
        }
        return true
    }

    def sum(M: Matrix) = {
        if (mat.length != M.length || mat.head.length != M.mat.head.length)
            throw new Exception("Matrici di dimensione diversa, impossibile fare la somma.")
        
        val newMat = new Matrix(mat.length, mat.head.length)
        
        for (i <- 0 until mat.length){
            for (j <- 0 until mat.head.length){
                newMat(i)(j) = mat(i)(j) + M(i)(j)
            }
        }
        newMat
    }

    def sub(M: Matrix) = sum(M.scalarMult(-1))

    def length = mat.length

    def nRows = mat.length
    def nCols = mat.head.length
    
    def getCol(n: Int) = mat.map(_(n))

    def getCols(ls: List[Int]) = {
        val newMat = new Matrix(0,0)
        for (i <- ls)
            newMat.addRow(getCol(i))
        newMat.T
        newMat
    }
    
    def inv = {
        val pw = new PrintWriter(new File("file.txt"))
        var s = "["
        for (i <- 0 until mat.length){
            s+= "["
            for (j <- 0 until mat.head.length){
                s+= mat(i)(j)
                if(j < mat.head.length-1)
                    s+=","
            }
            s+= "]"
            if (i < mat.length -1 )
                s+=","
        }
        s+="]"

        pw.write(s)
        pw.close
        val result = "python3 invert.py" ! ProcessLogger(stdout append _, stderr append _)
        val filename = "file.txt"

        val newMat : ListBuffer[ListBuffer[Double]] = ListBuffer()
        for (line <- Source.fromFile(filename).getLines) {
            newMat += line.replace("[","").replace("]","").trim.split(" +").to[ListBuffer].map(x => x.toDouble)
        }
        new Matrix(newMat.map(l => l.toList).toList)
    }

    def scalarMult(k: Float) =  new Matrix(mat.map(r => r.map(_ * k).toList).toList)

    def get(n: Int, m: Int) = mat(n)(m)

    def concat(M: Matrix) = {
        if (mat.length != M.length)
            throw new Exception("Matrici con numero di righe diverso, impossibile fare la concatenazione.")
        
        val newMat = new Matrix(0,0)
        for (i <- 0 until mat.length)
            newMat.addRow(mat(i) ++ M(i))

        newMat
    }

    def isPositive : Boolean = {
        for (i <- 0 until nRows)
            for (j <- 0 until nCols) 
                if (mat(i)(j) < 0)
                    return false
        return true
    }

    def leqZero: Boolean = {
        for (i <- 0 until nRows)
            for (j <- 0 until nCols) 
                if (mat(i)(j) > 0)
                    return false
        return true
    }

    def mult(M: Matrix) = {
        val rows = mat.length
        val cols = M.mat.head.length
        var newMat = new Matrix(rows, cols)
        for (i <- 0 until rows){
            for (j <- 0 until cols){
                var sum = 0.0
                for (k <- 0 until nCols)
                    sum = sum + mat(i)(k) * M(k)(j)
                newMat(i)(j) = sum
            }
        }
        newMat
    }
    
}

object MatrixTools {
    def identity(n: Int) = {
        val M = new Matrix(n, n)
        for (i <- 0 until n){
            for (j <- 0 until n){
                if (i == j)
                    M(i)(j) = 1
            }
        }
        M
    }

}