package logical

import scala.annotation.tailrec
import scala.collection.immutable.LongMap
import scala.language.implicitConversions

sealed abstract class LVar[A] {
  final def ===(that: LVar[A])(implicit unify: Unify[A]): Logic[Unit] =
    (this, that) match {
      case (LVar.Bound(x), LVar.Bound(y)) => unify(x, y)
      case (LVar.Unbound(k), LVar.Bound(v)) => LVar.bind(k, v)
      case (LVar.Bound(v), LVar.Unbound(k)) => LVar.bind(k, v)
      case (LVar.Unbound(x), LVar.Unbound(y)) => LVar.relate(x, y)
    }

  final def get: Logic[A] =
    this match {
      case LVar.Bound(v) =>
        Logic.Success(v)
      case LVar.Unbound(k) =>
        Logic.get.flatMap { case (_, vars) =>
          LVar.find(k, vars) match {
            case None => Logic.Failure
            case Some(v) => Logic.Success(v.asInstanceOf[A])
          }
        }
    }
}

object LVar {
  case class Unbound[A](key: Long) extends LVar[A]

  case class Bound[A](value: A) extends LVar[A]

  def apply[A]: Logic[LVar[A]] =
    Logic.get.flatMap { case (id, vars) =>
      Logic.put(id + 1, vars).map(_ => Unbound(id))
    }

  def find(key: Long, vars: LongMap[LVar[Any]]): Option[Any] = {
    @tailrec def loop(key: Long, path: Vector[Long]): Option[Any] =
      vars.get(key) match {
        case None => None
        case Some(LVar.Bound(v)) => Some(v)
        case Some(LVar.Unbound(k)) if path.contains(k) => None
        case Some(LVar.Unbound(k)) => loop(k, path :+ k)
      }
    loop(key, Vector(key))
  }

  def bind[A](key: Long, value: A)(implicit unify: Unify[A]): Logic[Unit] =
    Logic.get.flatMap { case (id, vars) =>
      find(key, vars) match {
        case None =>
          Logic.put(id, vars + (key -> LVar.Bound(value)))
        case Some(v) =>
          unify(value, v.asInstanceOf[A])
      }
    }

  def relate[A](x: Long, y: Long)(implicit unify: Unify[A]): Logic[Unit] =
    Logic.get.flatMap { case (id, vars) =>
      (find(x, vars), find(y, vars)) match {
        case (None, None) =>
          Logic.put(id, vars + (x -> LVar.Unbound[Any](y)) + (y -> LVar.Unbound[Any](x)))
        case (Some(v), None) =>
          Logic.put(id, vars + (y -> LVar.Bound(v)))
        case (None, Some(v)) =>
          Logic.put(id, vars + (x -> LVar.Bound(v)))
        case (Some(x), Some(y)) =>
          unify(x.asInstanceOf[A], y.asInstanceOf[A])
      }
    }

  implicit def wrap[A](a: A): LVar[A] = Bound(a)

  implicit def unify[A](implicit unify: Unify[A]): Unify[LVar[A]] =
    new Unify[LVar[A]] {
      def apply(x: LVar[A], y: LVar[A]): Logic[Unit] = x === y
    }
}
