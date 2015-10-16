package logical

import scala.annotation.tailrec

sealed trait Logic[A] {

  def run(value: A): Stream[A] = toDisj.apply(value)

  def &&&(that: => Logic[A]): Logic[A] =
    (this, that) match {
      case (term@Term(_), _) => And(term, () => True[A]) &&& that
      case (_, term@Term(_)) => this &&& And(term, () => True())
      case (True(), _) => that
      case (_, True()) => this
      case (False(), _) => False()
      case (_, False()) => False()
      case (xs@And(_, _), ys@And(_, _)) => xs.append(ys)
      case (Or(conj, disj), and@And(_, _)) => conj.append(and) ||| (disj() &&& and).toDisj
      case (and@And(_, _), Or(conj, disj)) => and.append(conj) ||| (and &&& disj()).toDisj
      case (Or(conj, disj), or@Or(_, _)) => conj &&& or ||| disj() &&& or
    }

  def |||(that: => Logic[A]): Logic[A] =
    this match {
      case term@Term(_) => And(term, () => True[A]) ||| that
      case True() => True()
      case and@And(_, _) => Or(and, () => that.toDisj)
      case False() => that
      case Or(conj, disj) => Or(conj, () => (disj() ||| that).toDisj)
    }

  def toDisj: Disj[A] =
    this match {
      case term@Term(_) => Or(And(term, () => True()), () => False())
      case True() => Or(True(), () => False())
      case False() => False()
      case and@And(_, _) => Or(and, () => False())
      case or@Or(_, _) => or
    }

}

case class Term[A](apply: A => A) extends Logic[A]

sealed trait Conj[A] extends Logic[A] {

  @tailrec
  final def apply(value: A): A =
    this match {
      case True() => value
      case And(term, conj) => conj().apply(term.apply(value))
    }

  def append(that: => Conj[A]): Conj[A] =
    this match {
      case True() => that
      case And(term, conj) => And(term, () => conj().append(that))
    }

}

case class And[A](term: Term[A], conj: () => Conj[A]) extends Conj[A]

case class True[A]() extends Conj[A]

sealed trait Disj[A] extends Logic[A] {

  def apply(value: A): Stream[A] =
    this match {
      case False() => Stream.empty
      case Or(conj, disj) => conj.apply(value) #:: disj().apply(value)
    }

  def append(that: => Disj[A]): Disj[A] =
    this match {
      case False() => that
      case Or(conj, disj) => Or(conj, () => disj().append(that))
    }

}

case class Or[A](conj: Conj[A], disj: () => Disj[A]) extends Disj[A]

case class False[A]() extends Disj[A]
