import scala.io.Source

object SimplexEvaluator {
    def main(args: Array[String]) = {   
        val src = scala.io.Source.fromFile(args(0))
        val in = src.mkString
        val p = new SimplexParser
        p.parseAll(p.problema, in.trim) match {
            case p.Success(tableau: Tableau, _) => 
                try {
                    tableau.simplexTableau
                } catch {
                    case e: ProblemaIllimitatoException => println("Problema illimitato.")
                    case e: BaseInamissibileException => println("Base inammissibile.")
                    case e: BaseDegenereException => println("Base degenere, da gestire.")
                }
            case x => print(x.toString)
        }
        src.close
    }
}