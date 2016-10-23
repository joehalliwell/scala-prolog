package com.joehalliwell.sp

import scala.annotation.tailrec
import scala.collection.immutable._

/*
 * An experiment with writing a Prolog interpreter in Scala
 * @author Joe Halliwell <joe.halliwell@gmail.com>
 */

object Prolog {
  val EmptyList = Atom("[]")
  val ListPredicate = "."
  val p = new Prolog()
}

class Prolog {
  var trace = false
  var database = scala.collection.mutable.Buffer[Term]()
  var operators = Seq[String](":-", ";", ",", "=", "is", "+", "-", "*", "/")
  var parser = new Parser(operators)

  //type Builtin = (Seq[Term], com.joehalliwell.sp.Env) => com.joehalliwell.sp.Env

  def assert(fact: Term) = {
    database += fact
  }

  def consult(filename: String) = {
    println("Loading " + filename)
    var lineNumber = -1;
    try {
      for (
        (line, index) <- scala.io.Source.fromFile(filename)
          .getLines.zipWithIndex
          .filter(!_._1.isEmpty)
          .filter(!_._1.startsWith("%"))
      ) {
        // Save the line number in case of error
        lineNumber = index
        assert(parser.parse(line.trim))
      }
    } catch {
      case e: Exception => {
        println("Syntax error in line " + (lineNumber + 1) + ": " + e.getMessage)
      }
    }
  }

  def write(term: Term, env: Success): Unit = term match {
    case v: Variable => env.binding.get(v) match {
      case Some(t: Term) => write(t, env)
      case None => print(v)
    }
    case default => print(term)
  }

  /**
   * Simple arithmetic expression evaluator.
   * TODO: Handle brackets? Can this be made tail-recursive?
   */
  private final def evaluate(term: Term, env: Success): Double = term match {
    case x: Number => x.value
    case v: Variable => env.binding.get(v) match {
      case None => throw new Exception("Unbound variable on RHS " + v);
      case Some(term) => evaluate(term, env)
    }
    case Predicate("+", 2, args) => evaluate(args.head, env) + evaluate(args.tail.head, env)
    case Predicate("*", 2, args) => evaluate(args.head, env) * evaluate(args.tail.head, env)
    case Predicate("-", 2, args) => evaluate(args.head, env) - evaluate(args.tail.head, env)
    case Predicate("/", 2, args) => evaluate(args.head, env) / evaluate(args.tail.head, env)
  }

  // TODO: Hide this implementation detail -- by returning an com.joehalliwell.sp.Env thunk?

  case class Stackframe(goal: Term, env: Success, index: Int)

  // Index scrolls through the database, but it also used as flag
  // to indicate that built-in procedures have been tried.
  // It is assumed they can only complete in one way.
  case class State(goals: Seq[Term], stack: Seq[Stackframe], index: Int) {
    def env = if (stack.isEmpty) Success() else stack.head.env

    def pop(): Option[State] = if (stack.isEmpty) None
    else stack.head.goal match {
      // HACK: Handle disjunctions
      case Predicate(";", 2, args) => Some(State(args.tail.head +: goals.tail, stack.tail, -1))
      case default => Some(State(stack.head.goal +: goals, stack.tail, stack.head.index + 1))
    }

    def push(): Option[State] = push(env, 0)

    def push(env: Env, index: Int): Option[State] = env match {
      case env: Success => Some(State(goals.tail, Stackframe(goals.head, env, index) +: stack, -1))
      case Fail => None
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
          case Fail => state.pop()
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
          case Fail => state.pop()
          case env: Success => state.push(env, -1)
        }
      }
      case default => Some(State(state.goals, state.stack, 0))
    }
  }

  def trySearch(state: State): Option[State] = {
    //println("Searching database..." + state.goals.head)
    for (index <- state.index until database.length) {
      database(index) match {
        // Rules
        case Predicate(":-", 2, args) => {
          state.env.unify((state.goals.head, args.head.renameVars(state.stack.length + 1))) match {
            case env: Success => {
              //println("Matched rule..." + Predicate(":-", 2, args))
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



}
