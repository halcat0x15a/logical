package logical

import scala.annotation.tailrec
import scala.collection.immutable.LongMap

abstract class Logic[+A] {
  def map[B](f: A => B): Logic[B] = flatMap(a => Logic.Success(f(a)))

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    this match {
      case Logic.Success(a) => f(a)
      case Logic.Failure => Logic.Failure
      case Logic.FlatMap(e, k) => Logic.FlatMap(e, (a: Any) => k(a).flatMap(f))
    }

  def split: Logic[Option[(A, Logic[A])]] = {
    def go(logic: Logic[A], stack: List[Logic[A]]) = loop(logic, stack)
    @tailrec def loop(logic: Logic[A], stack: List[Logic[A]]): Logic[Option[(A, Logic[A])]] =
      logic match {
        case Logic.Success(a) =>
          Logic.Success(Some((a, Logic.sum(stack))))
        case Logic.Failure =>
          stack match {
            case Nil => Logic.Success(None)
            case h :: t => loop(h, t)
          }
        case Logic.FlatMap(Logic.Choice, k) =>
          loop(k(true), k(false) :: stack)
        case Logic.FlatMap(effect, k) =>
          Logic.FlatMap(effect, (a: Any) => go(k(a), stack))
      }
    loop(this, Nil)
  }

  def once: Logic[A] =
    split.flatMap {
      case None => Logic.Failure
      case Some((logic, _)) => Logic.Success(logic)
    }

  def toStream: Stream[A] = {
    def loop(logic: Logic[A], id: Long, vars: LongMap[LVar[Any]]): Stream[A] =
      logic match {
        case Logic.Success(a) => Stream(a)
        case Logic.Failure => Stream.empty
        case Logic.FlatMap(Logic.Get, k) => loop(k((id, vars)), id, vars)
        case Logic.FlatMap(Logic.Put(id, vars), k) => loop(k(()), id, vars)
        case Logic.FlatMap(Logic.Choice, k) => loop(k(true), id, vars) ++ loop(k(false), id, vars)
      }
    loop(this, 0, LongMap.empty)
  }
}

object Logic {
  type State = (Long, LongMap[LVar[Any]])

  case object Failure extends Logic[Nothing]

  case class Success[A](value: A) extends Logic[A]

  case class FlatMap[A, B](effect: Effect[A], k: A => Logic[B]) extends Logic[B]

  abstract class Effect[+A]

  case object Get extends Effect[State]

  case class Put(id: Long, vars: LongMap[LVar[Any]]) extends Effect[Unit]

  case object Choice extends Effect[Boolean]

  val unit: Logic[Unit] = Success(())

  val get: Logic[State] = FlatMap(Get, (s: State) => Success(s))

  def put(id: Long, vars: LongMap[LVar[Any]]): Logic[Unit] = FlatMap(Put(id, vars), (_: Unit) => unit)

  def choice[A](x: => Logic[A], y: => Logic[A]): Logic[A] = FlatMap(Choice, (test: Boolean) => if (test) x else y)

  def sum[A](list: List[Logic[A]]): Logic[A] = list.foldRight(Failure: Logic[A])(_ ||| _)

  def find(key: Long, vars: LongMap[LVar[Any]]): Option[Any] = {
    @tailrec def loop(key: Long, path: Vector[Long]): Option[Any] =
      vars.get(key) match {
        case None =>
          None
        case Some(LVar.Bound(v)) =>
          Some(v)
        case Some(LVar.Unbound(k)) =>
          if (path.contains(k))
            None
          else
            loop(k, path :+ k)
      }
    loop(key, Vector(key))
  }

  def bind[A](key: Long, value: A)(implicit unify: Unify[A]): Logic[Unit] =
    get.flatMap { case (id, vars) =>
      find(key, vars).fold(put(id, vars + (key -> LVar.Bound(value)))) { v =>
        unify(value, v.asInstanceOf[A])
      }
    }

  def relate[A](x: Long, y: Long)(implicit unify: Unify[A]): Logic[Unit] =
    get.flatMap { case (id, vars) =>
      (find(x, vars), find(y, vars)) match {
        case (None, None) =>
          put(id, vars + (x -> LVar.Unbound[Any](y)) + (y -> LVar.Unbound[Any](x)))
        case (Some(v), None) =>
          put(id, vars + (y -> LVar.Bound(v)))
        case (None, Some(v)) =>
          put(id, vars + (x -> LVar.Bound(v)))
        case (Some(x), Some(y)) =>
          unify(x.asInstanceOf[A], y.asInstanceOf[A])
      }
    }

  implicit class LogicOps[A](val self: Logic[A]) extends AnyVal {
    def &&&[B](that: => Logic[B]): Logic[B] = self.flatMap(_ => that)

    def |||(that: => Logic[A]): Logic[A] = choice(self, that)
  }
}
