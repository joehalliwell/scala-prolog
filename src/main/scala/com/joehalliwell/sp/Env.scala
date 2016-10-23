package com.joehalliwell.sp

import scala.collection.immutable.Map

/** *****************************************************************
  *
  * ******************************************************************/

/*
 * Bindings
 * TODO: Occurs check? Merge with State?
 */
sealed trait Env {
    def unify(terms: (Term, Term)) = this
}

case object Fail extends Env

case class Success(binding: Map[Variable, Term] = Map[Variable, Term]()) extends Env {

    def bind(orig: Variable, v: Variable, t: Term): Env = {
        //println("B: " + v + "=" + t)
        binding.get(v) match {
            case None => Success(binding + (v -> t))
            case Some(v2) => v2 match {
                // Disallow circularities
                case v2: Variable => if (orig.equals(v2)) Fail else bind(orig, v2, t)
                case t2: Term if t.equals(t2) => this
                case default => unify(v2, t)
            }
        }
    }

    // Unify two terms, extending our bindings
    // TODO: Add note on case ordering (it's important!)
    override def unify(terms: (Term, Term)): Env = terms match {
        case (t1: Atom, t2: Atom) if t1 == t2 => this
        case (t1, t2: Variable) => bind(t2, t2, t1)
        case (t1: Variable, t2) => bind(t1, t1, t2)
        case (t1: Predicate, t2: Predicate) if t1.name == t2.name && t1.arity == t2.arity
            // This is neat, but inefficient! Is there a nice way to write it?
        => t1.args.zip(t2.args).foldLeft(this: Env)((x, y) => x.unify(y))
        case _ => Fail
    }

    //@tailrec
    def extract(t: Term): Term = t match {
        case Predicate(name, arity, args) => Predicate(name, arity, args.map(x => extract(x)))
        case v: Variable => binding.get(v) match {
            case None => v
            case Some(t) => extract(t)
        }
        case t => t
    }

    override def toString = {
        for {
            (variable, value) <- binding
            if variable.level == 0
        } yield variable.name + "=" + extract(value)
    } mkString "\n"

}
