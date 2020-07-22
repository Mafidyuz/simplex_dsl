import scala.io.Source

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