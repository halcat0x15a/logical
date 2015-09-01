package logical

import scala.collection.immutable.LongMap

case class Env(keys: LongMap[Long], values: LongMap[Any]) {

  def add(x: Long, y: Long): Env =
    copy(keys = keys + (x -> y) + (y -> x))

  def put(key: Long, value: Any): Env =
    copy(values = values + (key -> value))

  def get(key: Long): Option[Any] =
    values.get(key).orElse(keys.get(key).flatMap(values.get))

}

object Env {

  def add[A](x: Long, y: Long)(implicit A: Unify[A]): Logic[Unit] =
    new Logic[Unit] {
      def apply(env: Env): Stream[(Env, Unit)] =
        (env.get(x), env.get(y)) match {
          case (Some(x), Some(y)) => A.unify(x.asInstanceOf[A], y.asInstanceOf[A])(env)
          case (None, Some(value)) => Stream((env.put(x, value), ()))
          case (Some(value), None) => Stream((env.put(y, value), ()))
          case (None, None) => Stream((env.add(x, y), ()))
        }
    }

  def put[A](key: Long, value: A)(implicit A: Unify[A]): Logic[Unit] =
    new Logic[Unit] {
      def apply(env: Env): Stream[(Env, Unit)] =
        env.get(key).fold(Stream((env.put(key, value), ())))(v => A.unify(v.asInstanceOf[A], value)(env))
    }

  implicit val empty: Env = Env(LongMap.empty, LongMap.empty)

}
