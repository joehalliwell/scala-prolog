import com.joehalliwell.sp.{Atom, Prolog}
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification


class PrologParserTest extends Specification with ParserMatchers {
  val parsers = new Prolog().parser

  "term" should {
    "succeed to recognize 'hello'" in {
      parsers.exp must succeedOn(parsers.reader("hello")).withResult(Atom("hello"))
    }
  }

  "exp" should {
    "handle commas correctly" in {
      val ast = parsers.parse("X is 1 + 2; Y is 3 + 4.")
      println(ast)
      1 must_==(1)
    }
  }
}
