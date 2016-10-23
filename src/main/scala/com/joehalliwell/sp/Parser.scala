package com.joehalliwell.sp

import scala.collection.immutable.{Nil, Seq}
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader


class Parser(val operators: Seq[String]) extends RegexParsers with PackratParsers {

    val atom: Parser[Atom] = """!|[a-z]\w*""".r ^^ { Atom(_) }

    val number: Parser[Number] = """(\d+(\.\d+)?|\d*\.\d+)""".r ^^ { case v => Number(v.toDouble) }

    val str: Parser[Str] = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ {
        // TODO: Other escape codes
        case v => Str(v.replaceAll("\\\\n", "\n"))
    }

    val variable: Parser[Variable] = """[A-Z]\w*""".r ^^ { case name => Variable(name) }

//    def predicate: PackratParser[Predicate] =
//        """[a-z]\w*""".r ~ ("(" ~> rep1sep(exp, ",") <~ ")") ^^ {
//            case head ~ args => Predicate(head, args.length, args)
//        }

    def predicate: PackratParser[Predicate] =
        """[a-z]\w*""".r ~ ("(" ~> term <~ ")") ^^ {
            case head ~ arglist => {
                val args = unlist(arglist)
                Predicate(head, args.length, args)
            }
        }


    def list: Parser[Term] = "[" ~> listbody <~ "]"

    def listbody: Parser[Term] = repsep(exp, ",") ~ opt("|" ~> (variable | list)) ^^ {
        case headList ~ Some(tail) => list(headList, tail)
        case headList ~ None => list(headList, Prolog.EmptyList)
    }

    // TODO: Allow infix operators to be user defined
    lazy val exp: PackratParser[Term] = predicate | atom | variable | number | str | list
    lazy val term = infix(operators)
    lazy val sentence: PackratParser[Term] = term <~ "."

    def infix(ops: Seq[String]): Parser[Term] = {
        ops match {
            case Nil => exp | "(" ~> infix(operators) <~ ")"
            case op :: tail => {
                val lower = infix(tail)
                rep(lower ~ op) ~ lower ^^ {
                    // Implicit definition of operator associativity...
                    case left ~ right => left.reverse.foldLeft(right)((current, rest) => rest match {
                        case left ~ op => Predicate(op, 2, Seq(left, current))
                    })
                }
            }
        }
    }

    def parse(s: String): Term = phrase(sentence)(reader(s)) match {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg) // throws a runtime exception!
    }

    def reader(s: String) = new PackratReader(new CharSequenceReader(s))

    def unlist(list: Term): Seq[Term] = list match {
        case Predicate(",", 2, Seq(head, tail)) => head +: unlist(tail)
        case default => Seq(default)
    }

    // Helper function to make lists
    def list(args: Seq[Term], tail: Term): Term = args match {
        case Nil => tail
        case default => Predicate(Prolog.ListPredicate, 2, Seq(args.head, list(args.tail, tail)))
    }
}
