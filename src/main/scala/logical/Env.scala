package logical

case class Env(keySet: Set[Set[String]], valueMap: Map[String, Any]) {

  def add(keys: Set[String]): Env =
    keySet.find(_.exists(keys)).fold(copy(keySet = keySet + keys))(ks => copy(keySet = keySet - ks + (ks ++ keys)))

  def set[A](key: String, value: A): Env =
    copy(valueMap = valueMap + (key -> value))

  def get(key: String): Option[Any] =
    valueMap.get(key).orElse(keySet.find(_.contains(key)).flatMap(_.collectFirst(valueMap)))

}

object Env {

  implicit val empty: Env = Env(Set.empty, Map.empty)

}
