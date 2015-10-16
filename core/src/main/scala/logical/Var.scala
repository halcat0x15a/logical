/*package logical

import java.util.concurrent.atomic.AtomicLong

sealed abstract class Var[+A] {

  final def ===[B >: A](that: Var[B])(implicit B: Unify[B]): Logic[Unit] =
    (this, that) match {
      case (Bound(value1), Bound(value2)) => B.unify(value1, value2)
      case (Unbound(key), Bound(value)) => Env.put[B](key, value)
      case (Bound(value), Unbound(key)) => Env.put[B](key, value)
      case (Unbound(key1), Unbound(key2)) => Env.add[B](key1, key2)
    }

  final lazy val get: Logic[A] =
    this match {
      case Bound(value) => Logic.success(value)
      case Unbound(key) => Get(env => (env, env.get(key))).flatMap {
        case (env, Some(value)) => Put(env, value.asInstanceOf[A])
        case (_, None) => Failure
      }
    }

}

case class Unbound[A](key: Long) extends Var[A]

case class Bound[A](value: A) extends Var[A]

object Var {

  val counter: AtomicLong = new AtomicLong

  def apply[A]: Var[A] = Unbound(counter.getAndIncrement)

  def apply[A](value: A): Var[A] = Bound(value)

  implicit def unify[A](implicit A: Unify[A]): Unify[Var[A]] =
    new Unify[Var[A]] {
      def unify(x: Var[A], y: Var[A]): Logic[Unit] = x === y
    }

}
 */
