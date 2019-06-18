package logical

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.language.implicitConversions

sealed abstract class LVar[+A] {
  def ===[B >: A](that: LVar[B])(implicit unify: Unify[B]): Logic[Unit] =
    (this, that) match {
      case (LVar.Bound(x), LVar.Bound(y)) => unify(x, y)
      case (LVar.Unbound(k), LVar.Bound(v)) => LVar.bind(k, v)
      case (LVar.Bound(v), LVar.Unbound(k)) => LVar.bind(k, v)
      case (LVar.Unbound(x), LVar.Unbound(y)) => LVar.relate(x, y)
    }

  def get: Logic[A] =
    this match {
      case LVar.Bound(v) =>
        Logic.Success(v)
      case LVar.Unbound(k) =>
        Logic.get.flatMap { state =>
          LVar.find[A](k, state.vars) match {
            case None => Logic.Failure
            case Some(v) => Logic.Success(v)
          }
        }
    }
}

object LVar {
  case class Unbound(key: Int) extends LVar[Nothing]

  case class Bound[A](value: A) extends LVar[A]

  def apply[A]: Logic[LVar[A]] =
    for {
      state <- Logic.get
      _ <- Logic.put(state.copy(id = state.id + 1))
    } yield Unbound(state.id)

  def find[A](key: Int, vars: IntMap[LVar[Any]]): Option[A] = {
    @tailrec def loop(key: Int, path: Vector[Int]): Option[A] =
      vars.get(key) match {
        case None => None
        case Some(LVar.Bound(v)) => Some(v.asInstanceOf[A])
        case Some(LVar.Unbound(k)) if path.contains(k) => None
        case Some(LVar.Unbound(k)) => loop(k, path :+ k)
      }
    loop(key, Vector(key))
  }

  def bind[A](key: Int, value: A)(implicit unify: Unify[A]): Logic[Unit] =
    Logic.get.flatMap { state =>
      find[A](key, state.vars) match {
        case None =>
          Logic.put(state.copy(vars = state.vars + (key -> LVar.Bound(value))))
        case Some(v) =>
          unify(value, v)
      }
    }

  def relate[A](x: Int, y: Int)(implicit unify: Unify[A]): Logic[Unit] =
    Logic.get.flatMap { state =>
      (find[A](x, state.vars), find[A](y, state.vars)) match {
        case (None, None) =>
          Logic.put(state.copy(vars = state.vars + (x -> LVar.Unbound(y)) + (y -> LVar.Unbound(x))))
        case (Some(v), None) =>
          Logic.put(state.copy(vars = state.vars + (y -> LVar.Bound(v))))
        case (None, Some(v)) =>
          Logic.put(state.copy(vars = state.vars + (x -> LVar.Bound(v))))
        case (Some(x), Some(y)) =>
          unify(x, y)
      }
    }

  implicit def wrap[A](a: A): LVar[A] = Bound(a)

  implicit def unify[A](implicit unify: Unify[A]): Unify[LVar[A]] =
    new Unify[LVar[A]] {
      def apply(x: LVar[A], y: LVar[A]): Logic[Unit] = x === y
    }
}
