package slog

import java.util.UUID

case class Env(keyset: Set[Set[UUID]], values: Map[UUID, Any]) {

  def add(x: UUID, y: UUID): Env =
    if (x == y)
      this
    else
      copy(keyset = keyset + Set(x, y))

  def set[A](key: UUID, value: A)(implicit A: Unify[A]): Stream[Env] =
    values.get(key) match {
      case None => Stream(copy(values = values + (key -> value)))
      case Some(v) => A.unify(v.asInstanceOf[A], value)(this)
    }

  def keys(key: UUID): Set[UUID] =
    keyset.flatMap { pair =>
      if (pair(key))
        copy(keyset = keyset - pair).keys((pair - key).head)
      else
        Set.empty[UUID]
    } + key

  def get(key: UUID): Option[Any] = keys(key).collect(values).headOption

}
