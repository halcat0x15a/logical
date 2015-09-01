package logical

import java.util.concurrent.atomic.AtomicLong

sealed trait Var[+A] { self =>

  import Var._

  def ===[B >: A](that: Var[B])(implicit B: Unify[B]): Logic[Unit] =
    (this, that) match {
      case (Bound(x), Bound(y)) => B.unify(x, y)
      case (Unbound(x), Unbound(y)) => Env.add[B](x, y)
      case (Unbound(key), Bound(value)) => Env.put[B](key, value)
      case (Bound(value), Unbound(key)) => Env.put[B](key, value)
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

  private case class Unbound[A](id: Long) extends Var[A]

  private case class Bound[A](value: A) extends Var[A]

  private val counter: AtomicLong = new AtomicLong

  def apply[A]: Var[A] = Unbound(counter.getAndIncrement)

  def apply[A](value: A): Var[A] = Bound(value)

  implicit def unify[A](implicit A: Unify[A]): Unify[Var[A]] =
    new Unify[Var[A]] {
      def unify(x: Var[A], y: Var[A]): Logic[Unit] = x === y
    }

}
