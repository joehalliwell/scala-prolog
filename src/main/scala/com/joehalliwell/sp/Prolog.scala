import scala.collection.immutable._
import scala.tools.jline.console._
import scala.annotation.tailrec
import scala.util.continuations._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._

/*
 * An experiment with writing a Prolog interpreter in Scala
 * @author Joe Halliwell <joe.halliwell@gmail.com>
 */

object Prolog {
	val prompt = "\n? "
	val welcome = "Simple Prolog Interpreter in Scala"

	val EmptyList = Atom("[]")
	val ListPredicate = "."
	val p = new Prolog()		
	val console = new ConsoleReader()
	
	def main(args: Array[String]) = {
		
		console.setPrompt("? ")
		
		console.println(welcome)
		console.println("-" * welcome.length)
		
		p.consult("initial.pl")

		// The REPL
		while (true) {
			try {
				process(console.readLine())				
			} 
			catch { 
				case t: Throwable => t.printStackTrace();
			}
		}
	}

	def process(line: String) = line match {
		case "\\q" 	=> System.exit(0)
		case "\\l" 	=> console.println(p.database.mkString("\n"))
		case "\\?"	=> console.println("\\q\tquit\n\\l\tlist\n\\?\thelp\n")
		case "\\t"	=> {
			p.trace = !p.trace
			console.println("Tracing " + (if (p.trace) "on" else "off") + ".")
		}
		case _ 		=> prove(p.Parser.parse(line))
	}


	def prove(goal: Term) {
		val solution = p.State(Seq(goal), Seq(), -1)
		
    	@tailrec def next(solution: p.State): Unit = p.solve(solution) match {
			case None => {
				println("No.")
			}
			case Some(solution) => {
				println(solution.env)
				if (!solution.mayHaveMore) {
					println("Yes.")
				}
				else {
					console.print("More y/n")
					if (console.readLine().startsWith("y")) next(solution)
				}
			}
		}
		next(solution)
	}
}

class Prolog {
	var trace = false;
	var database = scala.collection.mutable.Buffer[Term]()
	var operators = scala.collection.mutable.Buffer[String](":-", ";", ",", "=", "is", "+", "-", "*", "/")

	//type Builtin = (Seq[Term], Env) => Env	

	def assert(fact: Term) = { 
		database += fact
	}

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
				assert(Parser.parse(line.trim))
			}
		} 
		catch { 
			case e: Exception => {
				println("Syntax error in line " + (lineNumber + 1) + ": " + e.getMessage)
			}
		}
	}

	def write(term: Term, env: Success): Unit = term match {
		case v: Variable => env.binding.get(v) match {
			case Some(t: Term) 	=> write(t, env)
			case None			=> print(v)
		}
		case default => print(term)
	}

	/**
	 * Simple arithmetic expression evaluator.
	 * TODO: Handle brackets? Can this be made tail-recursive?
	 */
	private final def evaluate(term: Term, env: Success): Double = term match {
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

	// TODO: Hide this implementation detail -- by returning an Env thunk?

	case class Stackframe(goal: Term, env: Success, index: Int)

	// Index scrolls through the database, but it also used as flag
	// to indicate that built-in procedures have been tried.
	// It is assumed they can only complete in one way.
	case class State(goals: Seq[Term], stack: Seq[Stackframe], index: Int) {
		def env 					= if (stack.isEmpty) Success() 	else stack.head.env
 		def pop():  Option[State] 	= if (stack.isEmpty) None 		else stack.head.goal match {
 			// HACK: Handle disjunctions
 			case Predicate(";", 2, args) => Some(State(args.tail.head +: goals.tail, stack.tail, -1))
 			case default => Some(State(stack.head.goal +: goals, stack.tail, stack.head.index + 1))
 		}
		def push(): Option[State] 	= push(env, 0)
		def push(env: Env, index: Int): Option[State] = env match {
			case env: Success => Some(State(goals.tail, Stackframe(goals.head, env, index) +: stack, -1))
			case Fail() => None
		}
		def isSolution = (goals.length == 0)
		def mayHaveMore = !env.binding.isEmpty
	}

	// Unused?
	def solve(term: Term): Option[State] = solve(State(Seq(term), Seq(), -1))

	// Find a solution
	def solve(initialState: State): Option[State] = {

		@tailrec def step(s: Option[State]): Option[State] = {
			if (trace) println(s)
			s match {
				case None => None
				case Some(s) if s.isSolution => Some(s)
				case Some(s) if s.index == -1 => step(tryBuiltins(s))
				case Some(s) => step(trySearch(s))
			}
		}

		step(if (initialState.isSolution) initialState.pop() else Some(initialState))
	}
	
	def tryBuiltins(state: State): Option[State] = {
		//if (state.index > -1) return trySearch(state) // HACK!
		//println("Checking for builtins...")
		state.goals.head match {
			case Atom("!") => {
				Some(State(state.goals.tail, Seq(Stackframe(Atom("!"), state.env, 0)), -1)) // No backtracking!
			}
			case Predicate("assert", 1, args) => {
				assert(args.head)
				state.push()
			}
			case Predicate("write", 1, args) => {
				write(args.head, state.env)
				state.push()
			}
			case Predicate("consult", 1, args) => {
				consult(args.head.toString)
				state.push()
			}
			case Predicate("=", 2, args) => {
				state.env.unify((args.head, args.tail.head)) match {
					case Fail() => state.pop()
					case env: Success => state.push(env, -1)
				}
			}
			case Predicate(",", 2, args) => {
				return Some(State(
					args ++ state.goals.tail,
					new Stackframe(state.goals.head, state.env, state.index) +: state.stack,
					-1))
			}
			case Predicate(";", 2, args) => {
				return Some(State(Seq(args.head) ++ state.goals.tail,
					new Stackframe(state.goals.head, state.env, state.index) +: state.stack,
					-1))
			}

			case Predicate("is", 2, args) => {
				var rhs = evaluate(args.tail.head, state.env)
				state.env.unify((args.head, Number(rhs))) match {
					case Fail() => state.pop()
					case env: Success => state.push(env, -1)
				}
			}
			case default => Some(State(state.goals, state.stack, 0))
		}
	}

	def trySearch(state: State): Option[State] = {
		//println("Searching database...")
		for (index <- state.index until database.length) {
			database(index) match {
				// Rules
				case Predicate(":-", 2, args) => { 
					state.env.unify((state.goals.head, args.head.renameVars(state.stack.length + 1))) match {
						case env: Success => {
							//println("Matched rule...")
							return Some(State(
								args.tail.map(_.renameVars(state.stack.length + 1)) ++ state.goals.tail, 
								new Stackframe(state.goals.head, env, index) +: state.stack, -1))
						}
						case default => //Option.empty[State]
					}
				}
				// Simple facts
				case fact => {
					state.env.unify((state.goals.head, fact.renameVars(state.stack.length + 1))) match {
						case env: Success => {
							//println("Matched fact...")
							//env.print()
							return state.push(env, index)
						}
						case default => //Option.empty[State]
					}
				}
			}
		}
		return state.pop()
	}

	/*******************************************************************
	 * PARSER
	 *******************************************************************/
	object Parser extends RegexParsers with PackratParsers {
		def atom:     Parser[Atom]    = """!|[a-z]\w*""".r ^^ { Atom(_) }
		def number:   Parser[Number]  = """(\d+(\.\d+)?|\d*\.\d+)""".r ^^ { case v => Number(v.toDouble) }
		def str:      Parser[Str]     = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~"\"" ^^ {
			// TODO: Other escape codes
			case v => Str(v.replaceAll("\\\\n","\n"))
		}
		def variable: 	Parser[Variable]					= """[A-Z]\w*""".r ^^ { case name => Variable(name) }
		def predicate:  PackratParser[Predicate]  = """[a-z]\w*""".r ~ ("(" ~> rep1sep(exp, ",") <~ ")") ^^ {
			case head ~ args => Predicate(head, args.length, args)
		}
		// TODO: Allow infix operators to be user defined
		lazy val infix: PackratParser[Predicate] = (term ~ infixOp ~ term) ^^ {
			case left ~ op ~ right => Predicate(op, 2, List(left, right))
		}
		def list:   		Parser[Term]    		= "[" ~> listbody <~ "]"
		def listbody:   Parser[Term]    		= repsep(exp, ",") ~ opt("|" ~> (variable | list)) ^^ {
			case headList ~ Some(tail)  => list(headList, tail)
			case headList ~ None    		=> list(headList, Prolog.EmptyList)
		}
		lazy val exp:			PackratParser[Term] = predicate | atom | variable | number | str | list
		lazy val term:    	PackratParser[Term] = infix | exp
		lazy val sentence:  PackratParser[Term]	= term <~ "."
		
		def infixOp:		Parser[String] = new Parser[String] {
	    def apply(in: Input) = {
	    	val source = in.source
	      val offset = in.offset
	      val start = handleWhiteSpace(source, offset)
	      val token = source.subSequence(start, source.length).toString()
	     	//println(operators)
	     	//println(token)
	    	operators.find(op => token.startsWith(op)) match {
	    		case Some(op) => Success(op, in.drop(start - offset + op.length))
	    		case None			=> Failure("Couldn't locate matching op in " + token, in.drop(start - offset)) 
	    	}
	    }
	  }

		def parse(s: String): Term = phrase(sentence)(reader(s)) match {
			case Success(result, _) => result
			case failure : NoSuccess => scala.sys.error(failure.msg) // throws a runtime exception!
		}

		def reader(s: String) = new PackratReader(new CharSequenceReader(s))

		// Helper function to make lists
		def list(args: Seq[Term], tail: Term): Term = args match {
			case Nil 			=> tail
			case default 	=> Predicate(Prolog.ListPredicate, 2, Seq(args.head, list(args.tail, tail)))
		}
	}

}


/*******************************************************************
 *
 *******************************************************************/

/*
 * Bindings
 * TODO: Occurs check? Merge with State?
 */
sealed trait Env {def unify(terms: (Term, Term)) = this }
case class Fail() extends Env
case class Success(binding: Map[Variable,Term] = Map[Variable,Term]()) extends Env {
	
	def bind(orig: Variable, v: Variable, t: Term): Env = {
		//println("B: " + v + "=" + t)	
		binding.get(v) match {
			case None => Success(binding + (v -> t))
			case Some(v2) => v2 match {
				// Disallow circularities
				case v2: Variable => if (orig.equals(v2)) Fail() else bind(orig, v2, t)
				case t2: Term if (t.equals(t2)) => this
				case default => unify(v2, t)
			}
		}
	}
	
	// Unify two terms, extending our bindings
	// TODO: Add note on case ordering (it's important!)
	override def unify(terms: (Term, Term)): Env = terms match {
		case (t1: Atom, t2: Atom) if (t1 == t2) => this
		case (t1, t2: Variable) => bind(t2, t2, t1)
		case (t1: Variable, t2) => bind(t1, t1, t2)
		case (t1: Predicate, t2: Predicate) if (t1.name == t2.name && t1.arity == t2.arity) 
			// This is neat, but inefficient! Is there a nice way to write it?
			=> t1.args.zip(t2.args).foldLeft(this: Env)((x,y) => x.unify(y))
		case _ => Fail()
	}

	def extract(t: Term): Term = t match {
		case Predicate(name, arity, args) => Predicate(name, arity, args.map(x => extract(x)))
		case v: Variable => binding.get(v) match {
			case None => v
			case Some(t) => extract(t)
		}
		case t => t
	}

	override def toString() = {
		binding.filter(x => x._1.level < 1).map(x => x._1.name + "=" + extract(x._2)).mkString("\n")
	}

}
