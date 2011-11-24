import scala.util.parsing.combinator._


// registers 0 - 15 are normal registers
// 16 is MAR
// 17 is MBR
class Value { def v: Int = -1}
case class Register(n: Int) extends Value {
	override def v = State.registers(n)
}

case class Addition(a: Register, b: Register) extends Value {
	override def v = State.registers(a.n) + State.registers(b.n)
}
case class LeftShift(a: Value) extends Value {
	override def v = a.v * 2
}
case class RightShift(a: Value) extends Value {
	override def v = a.v / 2
}

class Statement { def execute {} }

case class Label(name: String) extends Statement
case class Assignment(left: Register, right: Value) extends Statement {
	override def execute { State.registers(left.n) = right.v }
}
class FlowControl extends Statement { }
case class ifZero(condRegister: Register, negate: Boolean, target: Label) extends FlowControl {
	override def execute { 
		if(negate) {
			if( (~ condRegister.v) == 0)
				State.execPointer = State.labels(target.name)
		}
		else {
			if( condRegister.v == 0)
				State.execPointer = State.labels(target.name)
		}
	}
}
case class ifNegative(condRegister: Register, negate: Boolean, target: Label) extends FlowControl {
	override def execute { 
		if(negate) {
			if( (~ condRegister.v) < 0)
				State.execPointer = State.labels(target.name)
		}
		else {
			if( condRegister.v < 0)
				State.execPointer = State.labels(target.name)
		}
	}
}

case class StatementSequence(statements: List[Statement]) extends Statement {
	override def execute { statements.foreach(_.execute) }
}
case class FunctionName(name: String) extends Statement

class Micro16Parser extends JavaTokenParsers {
	def statement: Parser[Statement] =  ( 
			label 
			| flowControl 
			| assignment~";"~statement ^^ { 
				case ass~";"~(fun: StatementSequence) => StatementSequence(ass :: fun.statements)
				case ass~";"~fun => StatementSequence(List(ass, fun)) } 
			| assignment 
			| functionName 
			| failure("illegal statement"))
	def assignment: Parser[Assignment] = register~"<-"~value ^^ { case r~"<-"~v => Assignment(r, v) }
	def register: Parser[Register] = ( 
							"R"~"""\d{1,2}""".r ^^ { case "R"~num => Register(15.min(num.toInt)) }
							| "MAR" ^^ { case _ => Register(16) } 
							| "MBR" ^^ { case _ => Register(17) } )
	def value: Parser[Value] = register | function
	def expression: Parser[Value] = ( register~"+"~register ^^ { case r1~"+"~r2 => Addition(r1, r2) } 
		| register 
		| function )
	def function: Parser[Value] = ( "("~>expression<~")" 
		| "lsh("~>expression<~")" ^^ (LeftShift(_)) 
		| "rsh("~>expression<~")" ^^ (RightShift(_)) )
	def label: Parser[Label] = labelName<~":"
	def labelName: Parser[Label] = "[A-Z]".r ^^ (Label(_))
	def functionName: Parser[FunctionName] = "[a-z]+".r ^^ (FunctionName(_))
	def flowControl: Parser[FlowControl] = ( "("~negate~register~"); if"~condType~"goto"~labelName ^^ { 
		case "("~neg~reg~"); if"~condT~"goto"~label => 
			if(condT == "Z") 
				ifZero(reg, neg, label) 
			else 
				ifNegative(reg, neg, label) } )
	def negate: Parser[Boolean] = "-" ^^ (x => true) | "" ^^ (x => false)
	def condType: Parser[String] = "Z" | "N"
}

object State {
	val registers = (0 to 17).map(n => 0xDEADBEEF).toArray
	registers(0) =  0
	registers(1) =  1
	registers(2) = -1

	def dump() {
		println()
		println("+++++++ Dumping State +++++++")
		println("Execution Pointer: %02d".format(execPointer))
		println("====== Registers ======")
		registers.take(16).zipWithIndex.foreach(l => println(" %02d: %s".format(l._2, l._1)))
		println("MAR: %s".format(registers(16)))
		println("MBR: %s".format(registers(17)))
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