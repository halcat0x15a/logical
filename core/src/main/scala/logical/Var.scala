package logical

import java.util.concurrent.atomic.AtomicLong

sealed abstract class Var[+A] {

  def ===[B >: A](that: Var[B])(implicit B: Unify[B]): Logic[Unit]

  def get: Logic[A]

}

case class Unbound[A](key: Long) extends Var[A] {

  def ===[B >: A](that: Var[B])(implicit B: Unify[B]): Logic[Unit] =
    that match {
      case Unbound(k) => Env.add[B](key, k)
      case Bound(v) => Env.put[B](key, v)
    }

  def get: Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] =
        env.get(key).map(value => (env, value.asInstanceOf[A])).toStream
    }

}

case class Bound[A](value: A) extends Var[A] {

  def ===[B >: A](that: Var[B])(implicit B: Unify[B]): Logic[Unit] =
    that match {
      case Unbound(k) => Env.put[B](k, value)
      case Bound(v) => B.unify(value, v)
    }

  def get: Logic[A] = Logic.succeed(value)

}

object Var {

  val counter: AtomicLong = new AtomicLong

  def apply[A]: Var[A] = Unbound(counter.getAndIncrement)

  def apply[A](value: A): Var[A] = Bound(value)

  implicit def unify[A](implicit A: Unify[A]): Unify[Var[A]] =
    new Unify[Var[A]] {
      def unify(x: Var[A], y: Var[A]): Logic[Unit] = x === y
    }

}
