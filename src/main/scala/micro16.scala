package micro16

import scala.util.parsing.combinator._

// registers 0 - 15 are normal registers
// 16 is MAR
// 17 is MBR
class Value { def v: Short = -1}
case class Register(n: Short) extends Value { override def v: Short = State.registers(n) }
case class Number(n: Short) extends Value { override def v: Short = n }
case class Addition(a: Value, b: Value) extends Value {
	override def v: Short = (a.v + b.v).toShort
}
case class AND(a: Value, b: Value) extends Value {
	override def v: Short = (a.v & b.v).toShort
}
case class LeftShift(a: Value) extends Value { override def v: Short = (a.v << 1).toShort }
case class RightShift(a: Value) extends Value { override def v: Short = (a.v >> 1).toShort }
case class Negate(a: Value) extends Value { override def v: Short = (~a.v).toShort }

class Statement { def execute() {} }
case class Label(name: String) extends Statement
case class Assignment(left: Register, right: Value) extends Statement {
	override def execute() { State.registers(left.n) = right.v }
}
abstract class FlowControl(condRegister: Register, negate: Boolean, target: Label) extends Statement {
	override def execute() { 
		if( checkValue( if(negate) (~ condRegister.v).toShort else condRegister.v ))
			State.execPointer = State.labels(target.name)
	}
	def checkValue(n: Short): Boolean 
}
case class ifZero(cr: Register, n: Boolean, t: Label) extends FlowControl(cr, n, t) {
	override def checkValue(n: Short) = n == 0
}
case class ifNegative(cr: Register, n: Boolean, t: Label) extends FlowControl(cr, n, t) {
	override def checkValue(n: Short) = n < 0
}
case class Goto(target: Label) extends FlowControl(Register(0), false, target) {
	override def checkValue(n: Short) = true
}

case class StatementSequence(statements: List[Statement]) extends Statement {
	override def execute() { statements.foreach(_.execute) }
}
class MemoryAccess extends Statement
case class ReadMemory extends MemoryAccess {
	override def execute() { State.MBR = State.flash(State.MAR) }
}
case class WriteMemory extends MemoryAccess {
	override def execute() { State.flash(State.MAR) = State.MBR }
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
		| emptyLine
		| failure("illegal statement"))
	def assignment: Parser[Assignment] = register~"<-"~value ^^ { case r~"<-"~v => Assignment(r, v) }
	def register: Parser[Register] = ( 
		"R"~"""\d{1,2}""".r ^^ { case "R"~num => Register(State.pMaxRegister.min(num.toShort).toShort) }
		| "MAR" ^^ { case _ => Register(State.pMAR) } 
		| "MBR" ^^ { case _ => Register(State.pMBR) } )
	def value: Parser[Value] = number | register | function
	def number: Parser[Number] = ("-"~>"""\d+""".r ^^ (n => Number( (-1 * n.toShort).toShort )) 
	| """\d+""".r ^^ (n => Number(n.toShort)) )
	def expression: Parser[Value] = ( value~"+"~value ^^ { case r1~"+"~r2 => Addition(r1, r2) } 
		| value~"&"~value ^^ { case r1~"&"~r2 => AND(r1, r2) }
		| register 
		| function 
		| value )
	def function: Parser[Value] = ( "("~>expression<~")" 
		| "lsh("~>expression<~")" ^^ (LeftShift(_)) 
		| "rsh("~>expression<~")" ^^ (RightShift(_))
		| "~"~>register ^^ (Negate(_)))
	def label: Parser[Label] = labelName<~":"
	def labelName: Parser[Label] = "[A-Z][A-Z0-9_]*".r ^^ (Label(_))
	def memoryAccess: Parser[MemoryAccess] = "rd" ^^ (m => ReadMemory() ) | "wr" ^^ (m => WriteMemory() )
	def flowControl: Parser[FlowControl] = ( 
		"goto"~>labelName ^^ (Goto(_))
		| "("~negate~register~"); if"~condType~"goto"~labelName ^^ { 
			case "("~neg~reg~"); if"~condT~"goto"~label => 
				if(condT == "Z") 
					ifZero(reg, neg, label) 
				else 
					ifNegative(reg, neg, label) } )
	def negate: Parser[Boolean] = "~" ^^ (x => true) | "" ^^ (x => false)
	def condType: Parser[String] = "Z" | "N"
	def emptyLine: Parser[Statement] = """^\s*$""".r ^^ (s => new Statement())
}

object State {
	val pMaxRegister = 10
	val registers = new Array[Short](pMaxRegister + 3)
	val pMAR: Short = (pMaxRegister + 1).toShort
	val pMBR: Short = (pMaxRegister + 2).toShort
	def MAR = registers(pMAR)
	def MAR_= (newVal: Short) { registers(pMAR) = newVal }
	def MBR = registers(pMBR)
	def MBR_= (newVal: Short) { registers(pMBR) = newVal }

	// let's assume flash has a size of 1024 dwords
	val flash = new Array[Short](1024)

	var execPointer = 0
	val labels = new collection.mutable.HashMap[String, Short]()
	reset()

	def dump() = {
		def formatNum(n: Short) = {
			def getBinary(a: Short) = (0 to 15).map(n => (a >> n) & 1).reverse.mkString
			if(n == 0xDEAD.toShort)
				""
			else
				"%s = 0x%04X = %4d".format(
					getBinary(n).zipWithIndex.map(t => if(t._2 % 4 == 0) " " + t._1 else t._1).mkString.substring(1), 
					n, 
					n)
		}
		var out = ""
		out += "Execution Pointer: %02d\n".format(execPointer)
		out +="========== Registers ==========\n"
		registers.take(pMaxRegister+1).zipWithIndex.foreach(l => out +="R%02d: %s\n".format(l._2, formatNum(l._1)))
		out +="MAR: %s\n".format(formatNum(MAR))
		out +="MBR: %s\n".format(formatNum(MBR))
		out +="===== Beginning of flash =====\n"
		flash.take(20).zipWithIndex.foreach(l => out +=" %02d: %s\n".format(l._2, formatNum(l._1)))
		out
	}

	def reset() {
		(0 to (pMaxRegister + 2)).foreach(n => registers(n) = 0xDEAD.toShort)
		(0 to 1023).foreach(n => flash(n) = 0xDEAD.toShort)
		labels.clear()
		execPointer = 0
	}

}

object Micro16Simulator extends Micro16Parser {
	var codeGraph = new Array[Statement](0)

	def loadCode(code: String) {
		State.reset()
		codeGraph = code.split('\n').zipWithIndex.map( l => {
			var s = new Statement()
			val line = l._1.split("#")(0) // strip comments
			try { s = parseAll(statement, line).get }
			catch {
				case e: Exception => 
					println("Parsing line %d failed: %s".format(l._2 + 1, line))
					e.printStackTrace
			}
			s
			} ).toArray
	
		codeGraph.zipWithIndex.foreach( t => t._1 match { 
			case s: Label => State.labels(s.name) = t._2.toShort 
			case _ => } )
	}

	def printCode() {
		codeGraph.zipWithIndex.foreach(l => println("%02d: %s".format(l._2, l._1)))
	}

	def doStep() = {
		codeGraph(State.execPointer).execute()
		State.execPointer += 1
		State.execPointer
	}

	def canStep = State.execPointer < codeGraph.length

	def simulate() { while(canStep) doStep() }
}
