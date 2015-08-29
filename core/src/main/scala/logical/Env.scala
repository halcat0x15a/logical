package logical

import scala.collection.immutable.LongMap

case class Env(keySet: Set[Set[Long]], valueMap: LongMap[Any]) {

  def add(keys: Set[Long]): Env =
    keySet.find(_.exists(keys)).fold(copy(keySet = keySet + keys))(ks => copy(keySet = keySet - ks + (ks ++ keys)))

  def put(key: Long, value: Any): Env =
    copy(valueMap = valueMap + (key -> value))

  def get(key: Long): Option[Any] =
    valueMap.get(key).orElse(keySet.find(_.contains(key)).flatMap(_.collectFirst(valueMap)))

}

object Env {

  def add[A](x: Long, y: Long)(implicit A: Unify[A]): Logic[Env, Unit] =
    new Logic[Env, Unit] {
      def apply(env: Env): Stream[(Env, Unit)] =
        (env.get(x), env.get(y)) match {
          case (Some(x), Some(y)) => A.unify(x.asInstanceOf[A], y.asInstanceOf[A])(env)
          case _ => Stream((env.add(Set(x, y)), ()))
        }
    }

  def put[A](key: Long, value: A)(implicit A: Unify[A]): Logic[Env, Unit] =
    new Logic[Env, Unit] {
      def apply(env: Env): Stream[(Env, Unit)] =
        env.get(key).fold(Stream((env.put(key, value), ())))(v => A.unify(v.asInstanceOf[A], value)(env))
    }

  implicit val empty: Env = Env(Set.empty, LongMap.empty)

}
