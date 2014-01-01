import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import scala.collection.immutable._

object Parser extends RegexParsers with PackratParsers {
  def atom: 		Parser[Atom]		= """!|[a-z]\w*""".r ^^ { Atom(_) }
  def number:		Parser[Number]		= """(\d+(\.\d+)?|\d*\.\d+)""".r ^^ { case v => Number(v.toDouble) }
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
    case left ~ op ~ right => Predicate(op, 2, List(left, right))
  }
  def list:		Parser[Term] 		= "[" ~> listbody <~ "]"
  def listbody: 	Parser[Term] 		= repsep(term, ",") ~ opt("|" ~> (variable | list)) ^^ {
    case headList ~ Some(tail)	=> list(headList, tail)
    case headList ~ None		=> list(headList, Prolog.EmptyList)
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
    case default => Predicate(Prolog.ListPredicate, 2, Seq(args.head, list(args.tail, tail)))
  }
}