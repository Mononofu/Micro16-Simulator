
import scala.util.parsing.combinator._

// registers 0 - 15 are normal registers
// 16 is MAR
// 17 is MBR
class Value { def v: Int = -1}
case class Register(n: Int) extends Value
case class Label(name: String)

case class Addition(a: Register, b: Register) extends Value
case class LeftShift(a: Value) extends Value
case class RightShift(a: Value) extends Value

case class Assignment(left: Register, right: Value)

class Condition { }
case class ifZero(condRegister: Register, negate: Boolean, target: Label) extends Condition
case class ifNegative(condRegister: Register, negate: Boolean, target: Label) extends Condition

class Micro16 extends JavaTokenParsers {
	def statement: Parser[Any] =  ( label | flowControl | assignment~";"~statement ^^ { 
		case ass~";"~(fun: List[_]) => ass :: fun
		case ass~";"~fun => List(ass, fun)
		} | assignment | functionName )
	def assignment: Parser[Assignment] = register~"<-"~value ^^ { case r~"<-"~v => Assignment(r, v) }
	def register: Parser[Register] = "R"~"""\d{1,2}""".r ^^ { case "R"~num => Register(num.toInt)}  | "MAR" ^^ { case _ => Register(16) } | "MBR" ^^ { case _ => Register(17) }
	def value: Parser[Value] = register | function
	def expression: Parser[Value] = register~"+"~register ^^ { case r1~"+"~r2 => Addition(r1, r2) } | register | function
	def function: Parser[Value] = "("~>expression<~")" | "lsh("~>expression<~")" ^^ (LeftShift(_)) | "rsh("~>expression<~")" ^^ (RightShift(_))
	def label: Parser[Label] = labelName<~":"
	def labelName: Parser[Label] = "[A-Z]".r ^^ (Label(_))
	def functionName: Parser[String] = "[a-z]+".r
	def flowControl: Parser[Condition] = ( "("~negate~register~"); if"~condType~"goto"~labelName ^^ { 
		case "("~neg~reg~"); if"~condT~"goto"~label => 
			if(condT == "Z") ifZero(reg, neg, label) 
			else ifNegative(reg, neg, label) 
			} )
	def negate: Parser[Boolean] = "-" ^^ (x => true) | "" ^^ (x => false)
	def condType: Parser[String] = "Z" | "N"
}


object Booo extends Micro16 {
	def main(args: Array[String]) {
		println(parseAll(statement, "R4 <- (R1 + R1)"))
		println(parseAll(statement, "R4 <- lsh(R4 + R4)"))
		println(parseAll(statement, "L:"))
		println(parseAll(statement, "MAR <- (R3 + R4)"))
		println(parseAll(statement, "rd"))
		println(parseAll(statement, "MAR <- (R3 + R4); rd"))
		println(parseAll(statement, "R20 <- MBR"))
		println(parseAll(statement, "(-R4); if Z goto L"))
		println(parseAll(statement, "MAR <- R6; MBR <- R8; wr"))
	}
}