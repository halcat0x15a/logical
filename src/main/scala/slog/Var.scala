package slog

import java.util.UUID

sealed trait Var[A] {

  def ===(that: Var[A])(implicit A: Unify[A]): Logic =
    (this, that) match {
      case (Bound(x), Bound(y)) => A.unify(x, y)
      case (Unbound(key), Bound(value)) => new Logic { def apply(env: Env): Stream[Env] = env.set(key, value) }
      case (Bound(value), Unbound(key)) => new Logic { def apply(env: Env): Stream[Env] = env.set(key, value) }
      case (Unbound(x), Unbound(y)) => new Logic { def apply(env: Env): Stream[Env] = Stream(env.add(x, y)) }
    }

  def map[B](f: A => B): Var[B] =
    this match {
      case Unbound(id) => Unbound(id)
      case Bound(value) => Bound(f(value))
    }

}

case class Unbound[A](id: UUID) extends Var[A]

case class Bound[A](value: A) extends Var[A]

object Var {

  def apply[A]: Unbound[A] = Unbound(UUID.randomUUID)

  def apply[A](value: A): Bound[A] = Bound(value)

  def get[A](env: Env, v: Var[A]): Option[A] =
    v match {
      case Bound(value) => Some(value)
      case Unbound(id) => env.get(id).map(_.asInstanceOf[A])
    }

  implicit def unify[A](implicit A: Unify[A]): Unify[Var[A]] =
    new Unify[Var[A]] {
      def unify(x: Var[A], y: Var[A]): Logic = x === y
    }

}
