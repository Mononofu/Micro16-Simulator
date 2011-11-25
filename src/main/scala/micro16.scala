package micro16

import scala.util.parsing.combinator._

// registers 0 - 15 are normal registers
// 16 is MAR
// 17 is MBR
class Value { def v: Int = -1}
case class Register(n: Int) extends Value { override def v = State.registers(n) }
case class Number(n: Int) extends Value { override def v = n }
case class Addition(a: Register, b: Register) extends Value {
	override def v = State.registers(a.n) + State.registers(b.n)
}
case class LeftShift(a: Value) extends Value { override def v = a.v * 2 }
case class RightShift(a: Value) extends Value { override def v = a.v / 2 }
case class Negate(a: Value) extends Value { override def v = ~a.v }

class Statement { def execute() {} }
case class Label(name: String) extends Statement
case class Assignment(left: Register, right: Value) extends Statement {
	override def execute() { State.registers(left.n) = right.v }
}
abstract class FlowControl(condRegister: Register, negate: Boolean, target: Label) extends Statement {
	override def execute() { 
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
		| failure("illegal statement"))
	def assignment: Parser[Assignment] = register~"<-"~value ^^ { case r~"<-"~v => Assignment(r, v) }
	def register: Parser[Register] = ( 
		"R"~"""\d{1,2}""".r ^^ { case "R"~num => Register(State.pMaxRegister.min(num.toInt)) }
		| "MAR" ^^ { case _ => Register(State.pMAR) } 
		| "MBR" ^^ { case _ => Register(State.pMBR) } )
	def value: Parser[Value] = number | register | function
	def number: Parser[Number] = """\d+""".r ^^ (n => Number(n.toInt))
	def expression: Parser[Value] = ( register~"+"~register ^^ { case r1~"+"~r2 => Addition(r1, r2) } 
		| register 
		| function )
	def function: Parser[Value] = ( "("~>expression<~")" 
		| "lsh("~>expression<~")" ^^ (LeftShift(_)) 
		| "rsh("~>expression<~")" ^^ (RightShift(_))
		| "-"~>register ^^ (Negate(_)))
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
	val pMaxRegister = 15
	val registers = new Array[Int](pMaxRegister + 3)
	val pMAR = 16
	val pMBR = 17
	def MAR = registers(pMAR)
	def MAR_= (newVal: Int) { registers(pMAR) = newVal }
	def MBR = registers(pMBR)
	def MBR_= (newVal: Int) { registers(pMBR) = newVal }

	// let's assume flash has a size of 1024 dwords
	val flash = new Array[Int](1024)

	var execPointer = 0
	val labels = new collection.mutable.HashMap[String, Int]()
	reset()

	def dump() = {
		def formatNum(n: Int) = {
			if(n == 0xDEADBEEF)
				""
			else
				"0x%08x = %8d".format(n, n)
		}
		var out = ""
		out += "Execution Pointer: %02d\n".format(execPointer)
		out +="======== Registers ========\n"
		registers.take(pMaxRegister+1).zipWithIndex.foreach(l => out +="R%02d: %s\n".format(l._2, formatNum(l._1)))
		out +="MAR: %s\n".format(formatNum(MAR))
		out +="MBR: %s\n".format(formatNum(MBR))
		out +="=== Beginning of flash ===\n"
		flash.take(20).zipWithIndex.foreach(l => out +=" %02d: %s\n".format(l._2, formatNum(l._1)))
		out
	}

	def reset() {
		(0 to (pMaxRegister + 2)).foreach(n => registers(n) = 0xDEADBEEF)
		registers(0) =  0
		registers(1) =  1
		registers(2) = -1
		(0 to 1023).foreach(n => flash(n) = 0xDEADBEEF)
		labels.clear()
		execPointer = 0
	}

}

object Micro16Simulator extends Micro16Parser {
	var codeGraph = new Array[Statement](1)

	def loadCode(code: String) {
		State.reset()
		codeGraph = code.split('\n') .map( l => parseAll(statement, l).get ).toArray
	
		codeGraph.zipWithIndex.foreach( t => t._1 match { 
			case s: Label => State.labels(s.name) = t._2 
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
