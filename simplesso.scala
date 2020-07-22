import scala.util.parsing.combinator._
import scala.io.Source
import java.util.Scanner
import java.io.StringBufferInputStream
import scala.collection.mutable.ListBuffer

class Tableau(val min: Boolean, var c: Matrix, var coefficientiCostoRidotto: Matrix, var A: Matrix, var inBase: ListBuffer[Int], var fuoriBase: ListBuffer[Int], var terminiNoti: Matrix) {    

    var tableau = new Matrix(0,0)

    def printTableau = {
        coefficientiCostoRidotto.printMatrix
        tableau.printMatrix
        println()
    }

    def varEntrante: Int = { //regola di bland
        for(h <- 0 until coefficientiCostoRidotto(0).length){
            if((coefficientiCostoRidotto(0)(h) < 0 && min && fuoriBase.contains(h)) || (coefficientiCostoRidotto(0)(h) > 0 && !min && fuoriBase.contains(h)) )
                return h 
        }
        return -1
    } 

    def isOttimo: Boolean = {
        for(i <- 0 until coefficientiCostoRidotto(0).length){
            if((coefficientiCostoRidotto(0)(i) < 0 && fuoriBase.contains(i) && min) || (coefficientiCostoRidotto(0)(i) > 0 && fuoriBase.contains(i) && !min))
                return false
        } 
        return true
    }

    def argmin(h: Int): Int = {
        var A_h = tableau.getCol(h)
        var b_t = tableau.getCol(tableau(0).length - 1)
        
        var t = -1
        var argMin = Double.MaxValue

        for (i <- 0 until A_h.length){
            if (A_h(i) > 0 && argMin > (b_t(i) / A_h(i))){
                argMin = b_t(i) / A_h(i)
                t = inBase(i)
            }
        }
        return t
    }

    def passoPivot(h: Int, t: Int) = {
        val row = inBase.indexOf(t)
        val col = h
        var pivot = tableau(row)(col)
        println("Elemento di pivot: " + pivot + " row: " + (row+1) + " col: " + col )
        
        for (j <- 0 until tableau(0).length)
            tableau(row)(j) /= pivot
        
        for (i <- 0 until tableau.length){

            if (i != row){
                var mult = tableau(i)(col)
                for (j <- 0 until tableau(0).length)
                    tableau(i)(j) -= (tableau(row)(j) * mult) 
            }
        }
        var mult = coefficientiCostoRidotto(0)(col)
        for (j <- 0 until coefficientiCostoRidotto(0).length)
            coefficientiCostoRidotto(0)(j) -= (tableau(row)(j) * mult) 
    }

    def isIllimitato(h: Int) = tableau.getCol(h).forall(n => n <= 0) 

    def simplexTableau = {
        
        var illimitato = false

        var B = A.getCols(inBase.toList).T
        var BInv = B.inv
        
        
        var F = A.getCols(fuoriBase.toList).T
        
        var c_b = c.getCols(inBase.toList).T
        
        var z_b = c_b.mult(BInv).mult(terminiNoti)
        var risOttimo = z_b(0)(0)

        coefficientiCostoRidotto = (coefficientiCostoRidotto.concat(z_b))

        this.tableau = BInv.mult(F).concat(B.mult(BInv)).concat(terminiNoti)
        println("Tableau iniziale: ")
        while(isOttimo == false && illimitato == false){

            printTableau
            println("Variabili in base: " + inBase.map(n => "x"+(n+1)))
            println("Variabili fuori base: " + fuoriBase.map(n => "x"+(n+1)))
            
            var h = varEntrante
            
            if (isIllimitato(h)){
                illimitato = true
                throw new Exception("Problema Illimitato")
            }
            
            println("Variabile entrante: x" + (h+1))
            
            var t = argmin(h)
            println("Variabile uscente: x" + (t+1))
            
            passoPivot(h,t)
            println("=================================================================\n")

            inBase(inBase.indexOf(t)) = h 
            fuoriBase(fuoriBase.indexOf(h)) = t

        }
        B = tableau.getCols(inBase.toList).T
        BInv = B.inv
        
        F = tableau.getCols(fuoriBase.toList).T

        c_b = coefficientiCostoRidotto.getCols(inBase.toList).T
        
        var c_f = coefficientiCostoRidotto.getCols(fuoriBase.toList).T

        var u_t = c_b.mult(BInv)
        var ccr = c_f.sub(u_t.mult(F))
        println("\nVariabili in base: " + inBase.map(n => "x"+(n+1)))
        println("Variabili fuori base: " + fuoriBase.map(n => "x"+(n+1)))
        printTableau

        println("Ottimo trovato, z = " + (coefficientiCostoRidotto(0)(coefficientiCostoRidotto(0).length-1) * -1))
        
    }


    /*def simplex = {
        
        var illimitato = false
        var ottimo = false
        var risOttimo = 100.0

        while(ottimo == false && illimitato == false) {
            println(inBase)
            A.printMatrix
            var B = A.getCols(inBase.toList).T
            B.printMatrix
            var BInv = B.inv
            
            var F = A.getCols(fuoriBase.toList).T

            var c_b = c.getCols(inBase.toList).T
            
            var c_f = c.getCols(fuoriBase.toList).T

            var u_t = c_b.mult(BInv)
            var ccr = c_f.sub(u_t.mult(F))

            if (ccr.isPositive && min == true || !ccr.isPositive && min == false)
                ottimo = true
            
            else {
                var h = varEntrante(ccr)
                println("Variabile entrante: " + h)
                var A_ = F.mult(BInv)
                terminiNoti = BInv.mult(terminiNoti)

                if (A_.leqZero){
                    illimitato = true
                    println("Problema illimitato.")
                }
                else{
                    println("Termini noti: ")
                    terminiNoti.printMatrix

                    println("F * B^-1")
                    A_.printMatrix
                    //var t = A_.argmin(h, terminiNoti) + c_b.nCols
                    println("Variabile uscente: " + t)
                    //println("Argmin: " + t)
                    
                    inBase -= t 
                    inBase += h
                    fuoriBase += t
                    fuoriBase -= h

                }
            }
            var x_b = BInv.mult(terminiNoti)
            println("x_b")
            x_b.printMatrix
            println("c_b")
            c_b.printMatrix
            
            risOttimo = c_b.mult(x_b)(0)(0)
        }
        if (ottimo && !illimitato){
            val x_b =  
            println("Ottimo: " + risOttimo)
        }
    }*/
}


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

        val inBase = trovaBase.to[ListBuffer]
        val fuoriBase = List.range(0, A.nCols).filter(n => !inBase.contains(n)).to[ListBuffer]
        val c_ = new Matrix(List(c.slice(1, nvar + segni.length + 1)))//.mult(B.inv)
        val coefficientiCostoRidotto = c_
        val terminiNoti = new Matrix(List(b.toList).transpose)//.mult(B.inv)
        val tableau = new Tableau(min, c_, coefficientiCostoRidotto, A, inBase, fuoriBase, terminiNoti)
        tableau
    }

    def trovaBase = {
        var varBase : ListBuffer[Int] = ListBuffer()
        if (A.slice(A(0).length - A.length, A(0).length).equals(MatrixTools.identity(A.length))) {
/*            print("Base: ")
            print(List.range(A.length, A(0).length))*/
            List.range(A(0).length - A.length, A(0).length)
        }
        else {
            print("FALSO")
            List()
        }
    }

}

class SimplexParser extends JavaTokenParsers {
    val tableauBuilder = new TableauBuilder     
    def problema = (("min" | "max") <~ "z" <~ "=")  ~ funzioneObiettivo ~ ("vincoli" ~> vincoli) ^^ { 
        case "min" ~ _ ~ _ => 
            tableauBuilder.min = true
            val t = tableauBuilder.build
            t.simplexTableau
        case "max" ~ _ ~ _ => 
            tableauBuilder.min = false
            val t = tableauBuilder.build
            t.simplexTableau
    }   
    def funzioneObiettivo = riga ^^ {arr => tableauBuilder.c = arr}
    def vincoli = rep(riga ~ ("<=" | ">=" | "=") ~ tnoto) ^^ {
        case ls =>
            for (el <- ls){
                el match {
                    case (arr ~ segno ~ tn) => {
                        tableauBuilder.A.addRow(arr.to[ListBuffer])
                        tableauBuilder.segni += segno
                        tableauBuilder.b += tn
                    }
                }
            }
    }
    def tnoto = num
    def variabile = "x[0-9]+".r ^^ {_.charAt(1).toInt - '0'}
    def num = "-?[0-9]+".r ^^ {_.toInt} | "-" ^^ {_ => -1}
    def riga = (rep(singolo ~ ("+" | "-")) ~ singolo) ^^ {
        case ls ~ s => {
            val arr : Array[Double] = new Array(30)
            for (i <- 0 until ls.length){
                ls(i) match{
                    case ((n,v) ~ "+") => arr(v) = n
                    case ((n,v) ~ "-") => arr(v) = n;
                        if(i+1 < ls.length)   
                            println(ls(i+1))
                        else
                            s match {
                                case (a,b) => arr(b) = -a
                            }
                }
            }
            s match {
                case (n,v) => if (arr(v) == 0) arr(v) = n 
            }
            arr.toList
        }
    }
    def singolo = (opt(num) ~ variabile) ^^ {
        case Some(n) ~ v => (n,v)
        case None ~ v    => (1,v)
    }
}



object SimplexEvaluator {
    def main(args: Array[String]) = {   
        val src = scala.io.Source.fromFile(args(0))
        val in = src.mkString
        val p = new SimplexParser
        p.parseAll(p.problema, in.trim) match {
            case p.Success(s,_) => println(s)
            case x => print(x.toString)
        }
        src.close
    }
}