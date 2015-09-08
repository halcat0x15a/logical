package logical

import scala.collection.immutable.LongMap

case class Env(keys: LongMap[Set[Long]], values: LongMap[Any]) {

  def add(x: Long, y: Long): Env =
    keys.get(x).orElse(keys.get(y)).fold(copy(keys = keys + (x -> Set(x, y)) + (y -> Set(x, y)))) { ks =>
      val keyset = ks + x + y
      copy(keys = keyset.foldLeft(keys)((ks, k) => ks + (k -> keyset)))
    }

  def put(key: Long, value: Any): Env =
    keys.get(key).fold(copy(values = values + (key -> value))) { ks =>
      Env(keys = keys -- ks, values = ks.foldLeft(values)((vs, k) => vs + (k -> value)))
    }

  def get(key: Long): Option[Any] = values.get(key)

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
