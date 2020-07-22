import scala.collection.mutable.ListBuffer

class Tableau(val min: Boolean, var ccr: Matrix, var A: Matrix, var inBase: ListBuffer[Int], var fuoriBase: ListBuffer[Int], var terminiNoti: Matrix) {    

    var tableau = new Matrix(0,0)

    def printTableau = {
        ccr.printMatrix
        println()
        tableau.printMatrix
        println()
    }

    def varEntrante: Int = { //regola di bland
        for(h <- 0 until ccr(0).length){
            if((ccr(0)(h) < 0 && min && fuoriBase.contains(h)) || (ccr(0)(h) > 0 && !min && fuoriBase.contains(h)) )
                return h 
        }
        return -1
    } 

    def isOttimo: Boolean = { //Se non esistono coefficienti
        for(i <- 0 until ccr(0).length){
            if((ccr(0)(i) < 0 && fuoriBase.contains(i) && min) || (ccr(0)(i) > 0 && fuoriBase.contains(i) && !min))
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
        var mult = ccr(0)(col)
        for (j <- 0 until ccr(0).length)
            ccr(0)(j) -= (tableau(row)(j) * mult) 
    }

    def isIllimitato(h: Int) = tableau.getCol(h).forall(n => n <= 0) 

    def simplexTableau = {
        
        var illimitato = false

        var B = A.getCols(inBase.toList).T
        var BInv = B.inv
        
        var F = A.getCols(fuoriBase.toList).T
        
        var c_b = ccr.getCols(inBase.toList).T
        
        var z_b = c_b.mult(BInv).mult(terminiNoti)

        ccr = (ccr.concat(z_b))
        
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
        println("\nVariabili in base: " + inBase.map(n => "x"+(n+1)))
        println("Variabili fuori base: " + fuoriBase.map(n => "x"+(n+1)))
        printTableau

        println("Ottimo trovato, z = " + (ccr(0)(ccr(0).length-1) * -1))
        
    }
}
