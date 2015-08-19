package logical

import java.util.UUID

sealed trait Var[A] { self =>

  import Var._

  def ===(that: Var[A])(implicit A: Unify[A]): Logic[Unit] = {
    def set(key: String, value: A): Logic[Unit] =
      new Logic[Unit] {
        def apply(env: Env): Stream[(Env, Unit)] =
          env.get(key).fold(Stream((env.set(key, value), ())))(v => A.unify(v.asInstanceOf[A], value)(env))
      }
    (this, that) match {
      case (Bound(x), Bound(y)) => A.unify(x, y)
      case (Unbound(key), Bound(value)) => set(key, value)
      case (Bound(value), Unbound(key)) => set(key, value)
      case (Unbound(x), Unbound(y)) => new Logic[Unit] { def apply(env: Env): Stream[(Env, Unit)] = Stream((env.add(Set(x, y)), ())) }
    }
  }

  def get: Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] =
        self match {
          case Bound(value) => Stream((env, value))
          case Unbound(key) => env.get(key).map(value => (env, value.asInstanceOf[A])).toStream
        }
    }

}

object Var {

  private case class Unbound[A](name: String) extends Var[A]

  private case class Bound[A](value: A) extends Var[A]

  def apply[A]: Var[A] = Unbound(UUID.randomUUID.toString)

  def apply[A](value: A): Var[A] = Bound(value)

  implicit def unify[A](implicit A: Unify[A]): Unify[Var[A]] =
    new Unify[Var[A]] {
      def unify(x: Var[A], y: Var[A]): Logic[Unit] = x === y
    }

}
