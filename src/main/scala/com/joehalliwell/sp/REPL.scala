package com.joehalliwell.sp

import scala.annotation.tailrec
import scala.tools.jline.console.ConsoleReader
import scala.collection.immutable._


object REPL {

    val prompt = "\n? "
    val welcome =
        """|Simple Prolog Interpreter in Scala
          |
          |Type \? for help""".stripMargin


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
            } catch {
                case t: Throwable => t.printStackTrace();
            }
        }
    }

    def process(line: String) = line match {
        case "\\q" => System.exit(0)
        case "\\l" => console.println(p.database.mkString("\n"))
        case "\\?" => console.println("\\q\tquit\n\\l\tlist\n\\?\thelp\n")
        case "\\t" => {
            p.trace = !p.trace
            console.println("Tracing " + (if (p.trace) "on" else "off") + ".")
        }
        case _ => {
            val term = p.parser.parse(line)
            if (p.trace) println("Parse: " + term)
            prove(term)
        }
    }

    def prove(goal: Term) {
        val solution = p.State(Seq(goal), Seq(), -1)

        @tailrec def next(solution: p.State): Unit = {
            p.solve(solution) match {
                case None => {
                    println("No.")
                }
                case Some(solution) => {
                    println(solution.env)
                    if (!solution.mayHaveMore) {
                        println("Yes.")
                    } else {
                        console.print("More y/n")
                        if (console.readLine().startsWith("y")) next(solution)
                    }
                }
            }
        }
        next(solution)
    }

}
