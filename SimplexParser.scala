import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer

class SimplexParser extends JavaTokenParsers {
    val tableauBuilder = new TableauBuilder     
    def problema = (("min" | "max") <~ "z" <~ "=")  ~ funzioneObiettivo ~ ("vincoli" ~> vincoli) ^^ { 
        case "min" ~ _ ~ _ => {
            tableauBuilder.min = true
            tableauBuilder.build

        }   
        case "max" ~ _ ~ _ => {
            tableauBuilder.min = false
            tableauBuilder.build
        }
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