import scala.util.parsing.combinator._
import scala.collection.immutable._

// An experiment with writing a Prolog interpreter in Scala

object  Main {
	val prompt = "\n? "
	val welcome = "Simple Prolog Interpreter in Scala"

	def main(args: Array[String]) {
		println(welcome)
		println("-" * welcome.length)
		val p = new Prolog()		

		p.consult("initial.pl")
		print(prompt)

		for ( line <- io.Source.stdin.getLines ) {
			try {
					line match {
						case "\\q" 	=> System.exit(0)
						case "\\l" 	=> println(p.database.mkString("\n"))
						case _ 		=> p.solve(List(PrologParser.parse(line)), Success(), 1)
					}
			} catch { case e: Exception => println(e) }
			print(prompt)
		}
	}
}

class Prolog {

	val database = scala.collection.mutable.MutableList[Term]()

	def assert(fact: Term) { database += fact }

	def consult(filename: String) {
		println("Loading " + filename)
		var lineNumber = -1;
		try {
			for ((line, index) <- scala.io.Source.fromFile(filename)
					.getLines.zipWithIndex
					.filter(!_._1.isEmpty)
					.filter(!_._1.startsWith("%"))) 
			{
				// Save the line number in case of error
				lineNumber = index
				assert(PrologParser.parse(line.trim))
			}
		} 
		catch { 
			case e: Exception => {
				println("Syntax error in line " + lineNumber + ": " + e.getMessage)
			}
		}
	}

	def solve(goalList: Seq[Term], env: Env, level: Int): Unit = env match {
		case env: Fail => env // Fail fast
		case env: Success => goalList match {
			// We have a solution!
			case Nil => {
				env.print()
				println("More?")
				if (readLine().trim().startsWith("n")) throw new Exception("Interrupted")
				println("")
			}
			// Built-ins go here
			case Predicate("assert", 1, args) :: rest => {
				assert(args.head)
				solve(rest, env, level + 1)
			}
			// Depth-first search
			case goal :: rest => {
				//println("T: " + goal + " " + env)
				for (fact <- database) fact match {
					case Predicate("rule", _, args) => solve(
						args.tail.map(_.renameVars(level)) ++ rest,
						env.unify((goal, args.head.renameVars(level))),
						level + 1) 
					case default 					=> solve(rest, env.unify((goal, fact)), level + 1)
				}
				if (level == 1) println("No.")
			}
		}
	}
}

abstract class Env { def unify(terms: (Term, Term)) = this }
case class Fail() extends Env
case class Success(binding: Map[Variable,Term] = TreeMap[Variable,Term]()) extends Env {
	def bind(orig: Variable, v: Variable, t: Term): Env = binding.get(v) match {
		case None => Success(binding + (v -> t))
		case Some(v2) => v2 match {
				case v2: Variable => if (orig.equals(v2)) Fail() else bind(orig, v2, t)
				case t2: Term if (t.equals(t2)) => this
				case default => Fail()
		}
	}
	// Unify two terms, extending our bindings
	// TODO: Add note on case ordering (it's important!)
	override def unify(terms: (Term, Term)): Env = 
		{
			//println("U: " + terms + " in " + env)
			return terms match {
				case (t1: Atom, t2: Atom) if (t1 == t2) => this
				case (t1, t2: Variable) => bind(t2, t2, t1)
				case (t1: Variable, t2) => bind(t1, t1, t2)
				case (t1: Predicate, t2: Predicate) if (t1.name == t2.name && t1.adicity == t2.adicity) 
					// This is neat, but inefficient! Is there a nice way to write it?
					=> t1.args.zip(t2.args).foldLeft(this: Env)((x,y) => x.unify(y))
				case _ => Fail()
			}
		}
	def print() = {
		if (binding.size == 0) { println ("Yes") }
		else binding filterKeys(_.level == 0) foreach {
			case (k, v) => println(k + "=" + v)
		}
	}
}

// TERM TREE
abstract class Term {
	def renameVars(level: Int) = this
}
case class Atom(name: String) extends Term {
	override def toString = name
}
case class Number(value: Double) extends Term {
	override def toString = value.toString
}
case class Predicate(name: String, adicity: Int, args: Seq[Term]) extends Term {
	override def toString = name + "(" + args.mkString(",") + ")"
	override def renameVars(level: Int) = Predicate(name, adicity, args.map(_.renameVars(level)))
}
case class Variable(name: String, level: Int = 0) extends Term with Ordered[Variable] {
	override def toString = name //+ "_" + level
	override def renameVars(level: Int) = Variable(name, level)
	def compare(other: Variable): Int = name.compareTo(other.name) match {
		case 0 => level.compareTo(other.level)
		case v => v
	}
}

// PARSER
object PrologParser extends JavaTokenParsers {
	def atom: 		Parser[Atom]		= """[a-z]\w*""".r ^^ { Atom(_) }
	def number:		Parser[Number]		= """\d+(.\d+)?""".r ^^ { case v => Number(v.toDouble) }
	def variable:	Parser[Variable]	= """[A-Z]\w*""".r ^^ { case name => Variable(name) }
	def predicate: 	Parser[Predicate]	= """[a-z]\w*""".r ~ ("(" ~> repsep(term, ",") <~ ")") ^^ {
		case head ~ args => Predicate(head, args.length, List() ++ args)
	}
	def term:		Parser[Term]		= predicate | number | atom | variable
	def sentence:	Parser[Term] 		= term <~ "."

	def parse(s: String): Term = parseAll(sentence, s) match {
			case Success(result, _) => result
			case failure : NoSuccess => scala.sys.error(failure.msg) // throws a runtime exception!
		}
}