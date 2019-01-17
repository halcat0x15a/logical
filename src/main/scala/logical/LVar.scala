package logical

import java.util.concurrent.atomic.AtomicLong

sealed abstract class LVar[A] {
  def ===(that: LVar[A])(implicit unify: Unify[A]): Logic[Unit] =
    (this, that) match {
      case (LVar.Bound(x), LVar.Bound(y)) => unify(x, y)
      case (LVar.Unbound(k), LVar.Bound(v)) => Logic.bind(k, v)
      case (LVar.Bound(v), LVar.Unbound(k)) => Logic.bind(k, v)
      case (LVar.Unbound(x), LVar.Unbound(y)) => Logic.relate(x, y)
    }

  def get: Logic[A] =
    this match {
      case LVar.Bound(v) =>
        Logic.Success(v)
      case LVar.Unbound(k) =>
        Logic.get.flatMap { case (_, vars) =>
          Logic.find(k, vars) match {
            case None => Logic.Failure()
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

  implicit def wrap[A](a: A): LVar[A] = Bound(a)

  implicit def unify[A](implicit unify: Unify[A]): Unify[LVar[A]] =
    new Unify[LVar[A]] {
      def apply(x: LVar[A], y: LVar[A]): Logic[Unit] = x === y
    }
}
