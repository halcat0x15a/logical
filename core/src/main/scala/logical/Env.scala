package logical

import scala.annotation.tailrec
import scala.collection.immutable.LongMap

case class Env(keys: LongMap[Long], values: LongMap[Any]) {

  @tailrec
  final def add(key1: Long, key2: Long): Env =
    keys.get(key1) match {
      case None => copy(keys = keys + (key1 -> key2))
      case Some(key) => add(key, key2)
    }

  @tailrec
  final def put(key: Long, value: Any): Env = {
    val env = copy(values = values + (key -> value))
    keys.get(key) match {
      case Some(key0) => env.copy(keys = keys - key).put(key0, value)
      case None => env
    }
  }

  @tailrec
  final def get(key: Long): Option[Any] =
    values.get(key) match {
      case None =>
        keys.get(key) match {
          case None => None
          case Some(key0) => get(key0)
        }
      case result => result
    }

}

object Env {

  implicit val empty: Env = Env(LongMap.empty, LongMap.empty)

  def add[A](key1: Long, key2: Long)(implicit A: Unify[A]): Logic[Unit] =
    Get(env => (env, env.get(key1), env.get(key2))).flatMap {
      case (env, Some(value1), Some(value2)) => A.unify(value1.asInstanceOf[A], value2.asInstanceOf[A])
      case (env, None, Some(value)) => Put(env.put(key1, value), ())
      case (env, Some(value), None) => Put(env.put(key2, value), ())
      case (env, None, None) => Put(env.add(key1, key2), ())
    }

  def put[A](key: Long, value: A)(implicit A: Unify[A]): Logic[Unit] =
    Get(env => (env, env.get(key))).flatMap {
      case (env, opt) => opt.fold(Put(env.put(key, value), ()): Logic[Unit])(value0 => A.unify(value0.asInstanceOf[A], value))
    }

}
