import scala.util.parsing.combinator._


// registers 0 - 15 are normal registers
// 16 is MAR
// 17 is MBR
class Value { def v: Int = -1}
case class Register(n: Int) extends Value { override def v = State.registers(n) }
case class Addition(a: Register, b: Register) extends Value {
	override def v = State.registers(a.n) + State.registers(b.n)
}
case class LeftShift(a: Value) extends Value { override def v = a.v * 2 }
case class RightShift(a: Value) extends Value { override def v = a.v / 2 }

class Statement { def execute {} }
case class Label(name: String) extends Statement
case class Assignment(left: Register, right: Value) extends Statement {
	override def execute { State.registers(left.n) = right.v }
}
abstract class FlowControl(condRegister: Register, negate: Boolean, target: Label) extends Statement {
	override def execute { 
		if( checkValue( if(negate) (~ condRegister.v) else condRegister.v ))
			State.execPointer = State.labels(target.name)
	}
	def checkValue(n: Int): Boolean 
}
case class ifZero(cr: Register, n: Boolean, t: Label) extends FlowControl(cr, n, t) {
	override def checkValue(n: Int) = n == 0
}
case class ifNegative(cr: Register, n: Boolean, t: Label) extends FlowControl(cr, n, t) {
	override def checkValue(n: Int) = n < 0
}

case class StatementSequence(statements: List[Statement]) extends Statement {
	override def execute { statements.foreach(_.execute) }
}
class MemoryAccess extends Statement
case class ReadMemory extends MemoryAccess {
	override def execute { State.MBR = State.flash(State.MAR) }
}
case class WriteMemory extends MemoryAccess {
	override def execute { State.flash(State.MAR) = State.MBR }
}

class Micro16Parser extends JavaTokenParsers {
	def statement: Parser[Statement] =  ( 
		label 
		| flowControl 
		| assignment~";"~statement ^^ { 
			case ass~";"~(fun: StatementSequence) => StatementSequence(ass :: fun.statements)
			case ass~";"~fun => StatementSequence(List(ass, fun)) } 
		| assignment 
		| memoryAccess 
		| failure("illegal statement"))
	def assignment: Parser[Assignment] = register~"<-"~value ^^ { case r~"<-"~v => Assignment(r, v) }
	def register: Parser[Register] = ( 
		"R"~"""\d{1,2}""".r ^^ { case "R"~num => Register(State.maxRegister.min(num.toInt)) }
		| "MAR" ^^ { case _ => Register(State.pMAR) } 
		| "MBR" ^^ { case _ => Register(State.pMBR) } )
	def value: Parser[Value] = register | function
	def expression: Parser[Value] = ( register~"+"~register ^^ { case r1~"+"~r2 => Addition(r1, r2) } 
		| register 
		| function )
	def function: Parser[Value] = ( "("~>expression<~")" 
		| "lsh("~>expression<~")" ^^ (LeftShift(_)) 
		| "rsh("~>expression<~")" ^^ (RightShift(_)) )
	def label: Parser[Label] = labelName<~":"
	def labelName: Parser[Label] = "[A-Z]".r ^^ (Label(_))
	def memoryAccess: Parser[MemoryAccess] = "rd" ^^ (m => ReadMemory() ) | "wr" ^^ (m => WriteMemory() )
	def flowControl: Parser[FlowControl] = ( 
		"("~negate~register~"); if"~condType~"goto"~labelName ^^ { 
			case "("~neg~reg~"); if"~condT~"goto"~label => 
				if(condT == "Z") 
					ifZero(reg, neg, label) 
				else 
					ifNegative(reg, neg, label) } )
	def negate: Parser[Boolean] = "-" ^^ (x => true) | "" ^^ (x => false)
	def condType: Parser[String] = "Z" | "N"
}

object State {
	val maxRegister = 15
	val registers = (0 to (maxRegister + 2)).map(n => 0xDEADBEEF).toArray
	registers(0) =  0
	registers(1) =  1
	registers(2) = -1
	val pMAR = 16
	val pMBR = 17
	def MAR = registers(pMAR)
	def MAR_= (newVal: Int) { registers(pMAR) = newVal }
	def MBR = registers(pMBR)
	def MBR_= (newVal: Int) { registers(pMBR) = newVal }
	// let's assume flash has a size of 1024 dwords
	val flash = (0 to 1023).map(n => 0xDEADBEEF).toArray

	def dump() {
		println()
		println("+++++++ Dumping State +++++++")
		println("Execution Pointer: %02d".format(execPointer))
		println("====== Registers ======")
		registers.take(maxRegister+1).zipWithIndex.foreach(l => println(" %02d: %s".format(l._2, l._1)))
		println("MAR: %s".format(MAR))
		println("MBR: %s".format(MBR))
		println("====== Beginning of flash =====")
		flash.take(20).zipWithIndex.foreach(l => println(" %02d: %s".format(l._2, l._1)))
		println("+++++++++++++++++++++++++++++")
		println()
	}

	var execPointer = 0
	val labels = new collection.mutable.HashMap[String, Int]()
}

object Micro16Simulator extends Micro16Parser {
	def main(args: Array[String]) {
		val lines = ( if(args.length == 0) io.Source.stdin.getLines
		else io.Source.fromFile(args(0)).getLines )

		State.dump()
		val codeGraph = lines.map( l => parseAll(statement, l).get ).toArray
		codeGraph.zipWithIndex.foreach(l => println("%02d: %s".format(l._2, l._1)))	
		codeGraph.zipWithIndex.foreach( t => t._1 match { 
			case s: Label => State.labels(s.name) = t._2 
			case _ => } )
		while(State.execPointer < codeGraph.length) {
			val statement = codeGraph(State.execPointer)
			statement.execute
			State.execPointer += 1
		}
		State.dump()
	}
}