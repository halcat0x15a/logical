package logical

import scala.collection.immutable.IntMap

case class Env(keySet: Set[Set[Int]], valueMap: IntMap[Any]) {

  def add(keys: Set[Int]): Env =
    keySet.find(_.exists(keys)).fold(copy(keySet = keySet + keys))(ks => copy(keySet = keySet - ks + (ks ++ keys)))

  def set[A](key: Int, value: A): Env =
    copy(valueMap = valueMap + (key -> value))

  def get(key: Int): Option[Any] =
    valueMap.get(key).orElse(keySet.find(_.contains(key)).flatMap(_.collectFirst(valueMap)))

}

object Env {

  implicit val empty: Env = Env(Set.empty, IntMap.empty)

}
