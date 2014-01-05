import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
 
class PrologParserTest extends Specification with ParserMatchers {
  val parsers = new Prolog().Parser

  "term" should {
    "succeed to recognize 'hello'" in {
      parsers.term must succeedOn(parsers.reader("hello")).withResult(Atom("hello"))
    }
  }
}
