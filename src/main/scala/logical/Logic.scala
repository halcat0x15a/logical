package logical

import scala.annotation.tailrec
import scala.collection.immutable.LongMap

abstract class Logic[+A] {
  final def map[B](f: A => B): Logic[B] = flatMap(a => Logic.Success(f(a)))

  final def flatMap[B](f: A => Logic[B]): Logic[B] =
    this match {
      case Logic.Success(a) => f(a)
      case Logic.Failure => Logic.Failure
      case Logic.FlatMap(e, k) => Logic.FlatMap(e, (a: Any) => k(a).flatMap(f))
    }

  final def split: Logic[Option[(A, Logic[A])]] = {
    def go(logic: Logic[A], id: Long, vars: LongMap[LVar[Any]], stack: List[(Logic.State, Logic[A])]): Logic[Option[(A, Logic[A])]] = loop(logic, id, vars, stack)
    @tailrec def loop(logic: Logic[A], id: Long, vars: LongMap[LVar[Any]], stack: List[(Logic.State, Logic[A])]): Logic[Option[(A, Logic[A])]] =
      logic match {
        case Logic.Success(a) =>
          Logic.Success(Some((a, stack.foldRight(Logic.Failure: Logic[A]) {
            case (((id, vars), logic), acc) => Logic.put(id, vars).flatMap(_ => logic) ||| acc
          })))
        case Logic.Failure =>
          stack match {
            case Nil => Logic.Success(None)
            case ((id, vars), h) :: t => loop(h, id, vars, t)
          }
        case Logic.FlatMap(Logic.Choice, k) =>
          loop(k(true), id, vars, ((id, vars), k(false)) :: stack)
        case Logic.FlatMap(Logic.Get, k) =>
          loop(k(id, vars), id, vars, stack)
        case Logic.FlatMap(Logic.Put(id, vars), k) =>
          loop(k(()), id, vars, stack)
      }
    Logic.get.flatMap { case (id, vars) => loop(this, id, vars, Nil) }
  }

  final def once: Logic[A] =
    split.flatMap {
      case None => Logic.Failure
      case Some((a, _)) => Logic.Success(a)
    }

  final def cond[B](f: A => Logic[B])(z: => Logic[B]): Logic[B] =
    split.flatMap {
      case None => z
      case Some((h, t)) => f(h) ||| t.flatMap(f)
    }

  final def toStream: Stream[A] = {
    def loop(logic: Logic[A], id: Long, vars: LongMap[LVar[Any]]): Stream[A] =
      logic match {
        case Logic.Success(a) => Stream(a)
        case Logic.Failure => Stream.empty
        case Logic.FlatMap(Logic.Get, k) => loop(k((id, vars)), id, vars)
        case Logic.FlatMap(Logic.Put(id, vars), k) => loop(k(()), id, vars)
        case Logic.FlatMap(Logic.Choice, k) => loop(k(true), id, vars) #::: loop(k(false), id, vars)
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

  implicit class LogicOps[A](val self: Logic[A]) extends AnyVal {
    def &&&[B](that: => Logic[B]): Logic[B] = self.flatMap(_ => that)

    def |||(that: => Logic[A]): Logic[A] = choice(self, that)

    def interleave(that: => Logic[A]): Logic[A] =
      self.split.flatMap {
        case None => that
        case Some((h, t)) => Success(h) ||| that.interleave(t)
      }

    def orElse(alternative: => Logic[A]): Logic[A] = self.cond(Success(_))(alternative)
  }
}
