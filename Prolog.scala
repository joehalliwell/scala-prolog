import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import scala.collection.immutable._
import scala.tools.jline.console._

/*
 * An experiment with writing a Prolog interpreter in Scala
 * @author Joe Halliwell <joe.halliwell@gmail.com>
 */

object Prolog {
	val prompt = "\n? "
	val welcome = "Simple Prolog Interpreter in Scala"

	def main(args: Array[String]) = {
		println(welcome)
		println("-" * welcome.length)
		val p = new Prolog()		

		p.consult("initial.pl")

		val console = new ConsoleReader()
		console.setPrompt("? ")
		
		// The REPL
		while (true) {
			var line = console.readLine()
			try {
				line match {
					case "\\q" 	=> System.exit(0)
					case "\\l" 	=> println(p.database.mkString("\n"))
					case "\\?"	=> println("\\q\tquit\n\\l\tlist\n\\?\thelp\n")
					case _ 		=> p.solve(PrologParser.parse(line))
				}
			} 
			catch { case e: Exception => println(e) }
		}
	}
}


class Prolog {
	val database = scala.collection.mutable.MutableList[Term]()
	type Builtin = (Seq[Term], Env) => Env

	def assert(fact: Term) = { database += fact }

	def consult(filename: String) = {
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

	def write(term: Term, env: Success): Unit = term match 
	{
		case v: Variable => env.binding.get(v) match {
			case Some(t: Term) 	=> write(t, env)
			case None			=> print(v)
		}
		case default => print(term)
	}

	/**
	 * Simple arithmetic expression evaluator
	 */
	def evaluate(term: Term, env: Success): Double = term match {
		case x: Number 				=> x.value
		case v: Variable 			=> env.binding.get(v) match {
			case None				=> throw new Exception("Unbound variable on RHS");
			case Some(term)			=> evaluate(term, env)
		}
		case Predicate("+", 2, args) => evaluate(args.head, env) + evaluate(args.tail.head, env)
		case Predicate("*", 2, args) => evaluate(args.head, env) * evaluate(args.tail.head, env)
		case Predicate("-", 2, args) => evaluate(args.head, env) - evaluate(args.tail.head, env)
		case Predicate("/", 2, args) => evaluate(args.head, env) / evaluate(args.tail.head, env)
	}

	def solve(term: Term): Unit = solve(Seq(term), Success(), 1)

	// TODO: Rewrite as non-recursive?
	def solve(goalList: Seq[Term], env: Env, level: Int): Unit = {
		env match {
			case env: Fail => env // Fail fast
			case env: Success => {
				goalList match {
				// We have a solution!
				case Nil => {
					env.print()
					println("More y/n?")
					if (readLine().trim().startsWith("n")) throw new Exception("Interrupted")
					println("")
				}
				// Handle built-ins
				case Predicate("assert", 1, args) :: rest => {
					assert(args.head)
					solve(rest, env, level)
				}
				case Predicate("write", 1, args) :: rest => {
					write(args.head, env)
					solve(rest, env, level)
				}
				case Predicate("consult", 1, args) :: rest => {
					consult(args.head.toString)
					solve(rest, env, level)
				}
				case Predicate("=", 2, args) :: rest => {
					solve(rest, env.unify(args.head, args.tail.head), level)
				}
				case Predicate("is", 2, args) :: rest => {
					var rhs = evaluate(args.tail.head, env)
					solve(rest, env.unify(args.head, Number(rhs)), level)
				}
				// Depth-first search
				case goal :: rest => {
					//println("T: " + goal + " " + env)
					for (fact <- database) fact match {
						case Predicate("rule", _, args) => 
							solve(
								args.tail.map(_.renameVars(level)) ++ rest,
								env.unify((goal, args.head.renameVars(level))),
								level + 1) 
						case default 					=> 
							solve(rest, env.unify((goal, fact.renameVars(level))), level + 1)
					}
					if (level == 1) println("No.")
				}
			}
		}	
	}
	}
}

/*
 * Bindings
 */
sealed trait Env { def unify(terms: (Term, Term)) = this }
case class Fail() extends Env
case class Success(binding: Map[Variable,Term] = TreeMap[Variable,Term]()) extends Env {
	
	def bind(orig: Variable, v: Variable, t: Term): Env = {
		//println("B: " + v + "=" + t)	
		binding.get(v) match {
			case None => Success(binding + (v -> t))
			case Some(v2) => v2 match {
				case v2: Variable => if (orig.equals(v2)) Fail() else bind(orig, v2, t)
				case t2: Term if (t.equals(t2)) => this
				case default => unify(v2, t)
			}
		}
	}
	
	// Unify two terms, extending our bindings
	// TODO: Add note on case ordering (it's important!)
	override def unify(terms: (Term, Term)): Env = {
		return terms match {
			case (t1: Atom, t2: Atom) if (t1 == t2) => this
			case (t1, t2: Variable) => bind(t2, t2, t1)
			case (t1: Variable, t2) => bind(t1, t1, t2)
			case (t1: Predicate, t2: Predicate) if (t1.name == t2.name && t1.arity == t2.arity) 
				// This is neat, but inefficient! Is there a nice way to write it?
				=> t1.args.zip(t2.args).foldLeft(this: Env)((x,y) => x.unify(y))
			case _ => Fail()
		}
	}
	
	def print() = {
		val topLevel = binding.filter(x => x._1.level == 0)
		if (topLevel.size == 0) println ("Yes.")
		else topLevel.foreach { case (k, v) => println(k + "=" + v) }
	}
}

/**
 * AST for Prolog
 */
abstract class Term {
	def renameVars(level: Int) = this
}
case class Atom(name: String) extends Term {
	override def toString = name
}
case class Number(value: Double) extends Term {
	override def toString = value.toString
}
case class Str(value: String) extends Term {
	override def toString = value
}
case class Predicate(name: String, arity: Int, args: Seq[Term]) extends Term {
	override def toString = name match {
		case List.Predicate => "[" + unpackList().mkString(", ") + "]"
		case default 		=> name + "(" + args.mkString(", ") + ")"
	}
	override def renameVars(level: Int) = Predicate(name, arity, args.map(_.renameVars(level)))

	def unpackList() : Seq[Term] = args.tail.head match {
		case List.Empty 		=> Seq(args.head)
		case tail: Predicate 	=> Seq(args.head) ++ tail.unpackList()
		case default			=> Seq(args.head, Atom("|"), args.tail.head)
	}
}

case class Variable(name: String, level: Int = 0) extends Term with Ordered[Variable] {
	override def toString = name + "_" + level
	override def renameVars(level: Int) = Variable(name, level)
	def compare(other: Variable): Int = name.compareTo(other.name) match {
		case 0 => level.compareTo(other.level)
		case v => v
	}
}

object List {
	val Empty = Atom("[]")
	val Predicate = "."
}

// Parser
object PrologParser extends RegexParsers with PackratParsers {
	def atom: 		Parser[Atom]		= """[a-z]\w*""".r ^^ { Atom(_) }
	def number:		Parser[Number]		= """(\d+(\.\d*)?|\d*\.\d+)""".r ^^ { case v => Number(v.toDouble) }
	def str:		Parser[Str]			= "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~"\"" ^^ {
		// TODO: Other escape codes
		case v => Str(v.replaceAll("\\\\n","\n"))
	}
	def variable:	Parser[Variable]	= """[A-Z]\w*""".r ^^ { case name => Variable(name) }
	def predicate: 	Parser[Predicate]	= """[a-z]\w*""".r ~ ("(" ~> repsep(term, ",") <~ ")") ^^ {
		case head ~ args => Predicate(head, args.length, args)
	}
	// TODO: Allow infix operators to be user defined
	lazy val infix:	PackratParser[Predicate] = (term ~ """[-=+*/]|is""".r ~ term) ^^ {
		case left ~ op ~ right => Predicate(op, 2, Seq(left, right))
	}
	def list:		Parser[Term] 		= "[" ~> listbody <~ "]"
	def listbody: 	Parser[Term] 		= repsep(term, ",") ~ opt("|" ~> (variable | list)) ^^ {
		case headList ~ Some(tail)	=> list(headList, tail)
		case headList ~ None		=> list(headList, List.Empty)
	}
	lazy val term:		PackratParser[Term]		= predicate | infix | atom | variable | number | str | list
	lazy val sentence:	PackratParser[Term] 	= term <~ "."
	def parse(s: String): Term = phrase(sentence)(new PackratReader(new CharSequenceReader(s))) match {
		case Success(result, _) => result
		case failure : NoSuccess => scala.sys.error(failure.msg) // throws a runtime exception!	
	}

	// Helper function to make lists
	def list(args: Seq[Term], tail: Term): Term = args match {
		case Nil => tail
		case default => Predicate(List.Predicate, 2, Seq(args.head, list(args.tail, tail)))
	}
}