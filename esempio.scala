import scala.util.parsing.combinator._
import scala.io.Source
import java.util.Scanner
import java.io.StringBufferInputStream

trait Executable

class IncrementDataPointer extends Executable 
class DecrementDataPointer extends Executable
class IncrementByte extends Executable
class DecrementByte extends Executable
class OutputChar extends Executable
class InputChar extends Executable
class StatementList (body: List[Executable]) extends Executable {
    def getList = body
}
class Loop(body: StatementList) extends Executable {
    def getList = body.getList
}
class SpuriousString extends Executable



class Program(body: StatementList) {
    var mem = new Array[Byte](30000)
    var ptr = 0
    var stream = new StringBufferInputStream("")
    val scanner = new Scanner(System.in)

    def run(e: Executable) : Unit = e match {
        case _: IncrementDataPointer => ptr += 1
        case _: DecrementDataPointer => ptr -= 1
        case _: IncrementByte => mem(ptr) = (mem(ptr) + 1).toByte
        case _: DecrementByte => mem(ptr) = (mem(ptr) - 1).toByte
        case _: OutputChar => print(mem(ptr).toChar)
        case _: InputChar => 
            if(stream.available > 0) 
                mem(ptr) = stream.read.toByte
            else {
                stream = new StringBufferInputStream(scanner.nextLine)
                mem(ptr) = stream.read.toByte
            }
        case s: StatementList => s.getList.foreach(run(_))
        case l: Loop => while(mem(ptr) != 0) l.getList.foreach(run(_))
        case _: SpuriousString => ;
    }

    def execute = run(body)
}

class BrainFuckParser extends JavaTokenParsers {
    def program = statementList ^^ {sl => new Program(sl)}
    def statementList: Parser[StatementList] = rep(statement) ^^ {(ls: List[Executable]) => new StatementList(ls)}
    def statement = (">" | "<" | "+" | "-" | "." | "," | "[" ~> statementList <~ "]" | spuriousString) ^^ {
        case ">" => new IncrementDataPointer
        case "<" => new DecrementDataPointer
        case "+" => new IncrementByte
        case "-" => new DecrementByte
        case "." => new OutputChar
        case "," => new InputChar
        case s: StatementList => new Loop(s)
        case _ => new SpuriousString
    }
    def spuriousString = """[^><+-\.,\]\[]+""".r ^^ {_ =>  }
}

val parser = new BrainFuckParser
val src = scala.io.Source.fromFile("hw.bf")
parser.parseAll(parser.program, src.mkString) match {
    case parser.Success((res: Program), _) => res.execute
    case s => println(s)
}
src.close