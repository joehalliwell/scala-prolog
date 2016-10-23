package com.joehalliwell.sp

import scala.collection.immutable._

/**
  * AST for Prolog
  */
sealed trait Term {
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
        case Prolog.ListPredicate => "[" + unpackList().mkString(", ") + "]"
        case default => name + "(" + args.mkString(", ") + ")"
    }

    override def renameVars(level: Int) = Predicate(name, arity, args.map(_.renameVars(level)))

    def unpackList(): Seq[Term] = args.tail.head match {
        case Prolog.EmptyList => Seq(args.head)
        case tail: Predicate => Seq(args.head) ++ tail.unpackList()
        case default => Seq(args.head, Atom("|"), args.tail.head)
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
