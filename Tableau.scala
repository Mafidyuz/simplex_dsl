import scala.collection.mutable.ListBuffer

class Tableau(var min: Boolean, var primaFase: Boolean, var ccr: Matrix, var dueFasiCcr: Matrix, var A: Matrix, var inBase: ListBuffer[Int], var fuoriBase: ListBuffer[Int], var terminiNoti: Matrix) {    
    
    var B = A.getCols(inBase.toList).T
    var BInv = B.inv
    
    var F = A.getCols(fuoriBase.toList).T
    
    var c_b = ccr.getCols(inBase.toList).T
    
    var z_b = c_b.mult(BInv).mult(terminiNoti)

    var minDueFasi = true

    ccr = ccr.concat(z_b)
    
    var tableau = BInv.mult(F).concat(B.mult(BInv)).concat(terminiNoti)

    def printNomeVariabile(x: Int) = {
        if (primaFase && x + 1 >= ccr.nCols - A.nRows) 
            print("y"+ (x - ccr.nCols + A.nRows + 2) + " ")
        else 
            print("x"+ (x + 1) + " ")
    }

    def getNomiVariabiliInBase = {
        var ls : ListBuffer[String]= ListBuffer()
        for (x <- inBase){
            if (primaFase && x + 1 >= ccr.nCols - A.nRows) 
                ls += ("y"+ (x - ccr.nCols + A.nRows + 2))
            else 
                ls += ("x"+ (x + 1))
        }
        ls.toList
    }

    def printTableau = {
        print("T\t")
        if(primaFase){
            for(i <- 1 until ccr.nCols - A.nRows)
            print("x" + i + "\t")
            for(i <- 1 until A.nRows + 1)
            print("y" + i + "\t")
        }
        else {
            for(i <- 1 until ccr.nCols)
            print("x" + i + "\t")
        }
        println("-z")
        print("ccr\t")
        ccr.printMatrix
        println()
        tableau.printMatrixWithVariables(getNomiVariabiliInBase)
        println()
        /*print("Variabili in base: ")
        for (i <- 0 until inBase.length){
            printNomeVariabile(inBase(i))
        }
        println()
        print("Variabili fuori base: " )
        for (i <- 0 until fuoriBase.length){
            printNomeVariabile(fuoriBase(i))
        }
        println()*/

    }

    //operazioni di pivot per rendere canonica la prima riga del tableau
    def canonicizza_ccr = {
        ccr(0)(ccr.nCols - 1) = 0
        println("Tableau non canonico rispetto alla funzione obiettivo: ")
        printTableau
        println("=================================================================\n")
        for (i <- 0 until inBase.length){
            if (ccr(0)(inBase(i)) != 0) {
                var mult = ccr(0)(inBase(i))
                for(j <- 0 until ccr.nCols){
                    ccr(0)(j) -= (tableau.concat(terminiNoti)(i)(j) * mult)
                }
            }
        }
        if(primaFase)
            ccr(0)(ccr.nCols - 1) = z_b(0)(0) * -1
    }

    //scelgo la variabile entrante con la regola di bland
    def varEntrante: Int = { 
        for(h <- 0 until ccr.nCols){
            if((ccr(0)(h) < 0 && min && fuoriBase.contains(h)) || (ccr(0)(h) > 0 && !min && fuoriBase.contains(h)) )
                return h 
        }
        throw new RegolaDiBlandException
    } 

    //Se non esistono coefficienti di costo ridotto fuori base negativi (min) o positivi (max), siamo in condizioni di ottimalità 
    def isOttimo: Boolean = {
        for(i <- 0 until ccr.nCols){
            if((ccr(0)(i) < 0 && fuoriBase.contains(i) && min) || (ccr(0)(i) > 0 && fuoriBase.contains(i) && !min))
                return false
        } 
        return true
    }

    //scelgo l'argomento minimo candidato per uscire dalla base corrente con la regola di bland
    def argmin(h: Int): Int = {
        var A_h = tableau.getCol(h)
        var b_t = tableau.getCol(tableau.nCols - 1)
        
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

    //passo di pivot con elemento di pivot A_{t,h}
    def passoPivot(h: Int, t: Int) = {
        val row = inBase.indexOf(t)
        val col = h
        var pivot = tableau(row)(col)
        println("Elemento di pivot: " + pivot + " row: " + (row+1) + " col: " + col )
        
        for (j <- 0 until tableau.nCols)
            tableau(row)(j) /= pivot
        
        for (i <- 0 until tableau.length){
            if (i != row){
                var mult = tableau(i)(col)
                for (j <- 0 until tableau.nCols)
                    tableau(i)(j) -= (tableau(row)(j) * mult) 
            }
        }
        var mult = ccr(0)(col)
        for (j <- 0 until ccr.nCols)
            ccr(0)(j) -= (tableau(row)(j) * mult) 
    }

    //Se tutti gli elementi della colonna h sono <= 0, problema illimitato
    def isIllimitato(h: Int) = tableau.getCol(h).forall(n => n <= 0) 
    
    //Simplesso utilizzando il tableau
    def simplexTableau: Unit = {
        if (primaFase) {
            minDueFasi = min 
            min = true
        }
        
        println("Tableau iniziale: ")

        while(isOttimo == false){

            printTableau
            
            var h = varEntrante
            
            if (isIllimitato(h)) throw new ProblemaIllimitatoException
            
            print("Variabile entrante: " )
            printNomeVariabile(h)
            println()
            
            var t = argmin(h)
            print("Variabile uscente: ")
            printNomeVariabile(t)
            println()
            
            passoPivot(h,t)

            inBase(inBase.indexOf(t)) = h 
            fuoriBase(fuoriBase.indexOf(h)) = t
            println("=================================================================\n")
        }
        printTableau
        println("=================================================================\n")


        println("Ottimo trovato, z = %.2f".format(ccr(0)(ccr.nCols-1) * -1))

        if (primaFase){
            println("========================= SECONDA FASE ==========================")
            secondaFase
        }
    }

    //Seconda fase del metodo a due fasi
    def secondaFase = {
        //se z_b != 0, base inammissibile
        if (Math.round((ccr(0)(ccr.nCols - 1)*100.0)/100.0) != 0 ){ 
            throw new BaseInamissibileException
        }
        //Se z_b == 0 ma abbiamo variabili y in base, base degenere. In questo caso il problema è gestibile
        else if (inBase.exists(n => n > A.nCols)) { 
            throw new BaseDegenereException //TODO: Gestire basi degeneri
        }
        //se z_b == 0 e non abbiamo variabili y in base partiamo con la seconda fase
        else {
            min = minDueFasi
            primaFase = false
            tableau = tableau.getCols(List.range(0,A.nCols - A.length) ++ List(tableau.nCols - 1)).T
            
            fuoriBase = fuoriBase.filter(n => n < A.nCols - A.length)
            
            ccr = dueFasiCcr.concat(new Matrix(List(List(0))))
            canonicizza_ccr
            simplexTableau
        }
    }
}
