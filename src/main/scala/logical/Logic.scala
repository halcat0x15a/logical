package logical

import scala.annotation.tailrec
import scala.collection.immutable.IntMap

sealed abstract class Logic[+A] {
  def map[B](f: A => B): Logic[B] = flatMap(a => Logic.Success(f(a)))

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    this match {
      case Logic.Success(a) => f(a)
      case Logic.Failure => Logic.Failure
      case Logic.FlatMap(e, k) => Logic.FlatMap(e, k :+ f)
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap(_ => that)

  def |||[B >: A](that: => Logic[B]): Logic[B] = Logic.choose(this, that)

  def withFilter(p: A => Boolean): Logic[A] =
    flatMap(a => if (p(a)) Logic.Success(a) else Logic.Failure)

  def split: Logic[Option[(A, Logic[A])]] = {
    def go(logic: Logic[A], state: Logic.State, stack: List[(Logic.State, Logic[A])]): Logic[Option[(A, Logic[A])]] = loop(logic, state, stack)
    @tailrec def loop(logic: Logic[A], state: Logic.State, stack: List[(Logic.State, Logic[A])]): Logic[Option[(A, Logic[A])]] =
      logic match {
        case Logic.Success(a) =>
          Logic.Success(Some((a, stack.foldRight(Logic.Failure: Logic[A]) {
            case ((state, logic), acc) => Logic.put(state).flatMap(_ => logic) ||| acc
          })))
        case Logic.Failure =>
          stack match {
            case Nil => Logic.Success(None)
            case (state, h) :: t => loop(h, state, t)
          }
        case Logic.FlatMap(Logic.Choose, k) =>
          loop(k(true), state, (state, k(false)) :: stack)
        case Logic.FlatMap(Logic.Get, k) =>
          loop(k(state), state, stack)
        case Logic.FlatMap(state: Logic.State, k) =>
          loop(k(()), state, stack)
      }
    Logic.get.flatMap(loop(this, _, Nil))
  }

  def once: Logic[A] =
    split.flatMap {
      case None => Logic.Failure
      case Some((a, _)) => Logic.Success(a)
    }

  def cond[B](f: A => Logic[B])(z: => Logic[B]): Logic[B] =
    split.flatMap {
      case None => z
      case Some((h, t)) => f(h) ||| t.flatMap(f)
    }

  def orElse[B >: A](alternative: => Logic[B]): Logic[B] = cond(Logic.Success(_: B))(alternative)

  def interleave[B >: A](that: => Logic[B]): Logic[B] =
    split.flatMap {
      case None => that
      case Some((h, t)) => Logic.Success(h) ||| that.interleave(t)
    }

  def toVector: Vector[A] = {
    @tailrec def loop(logic: Logic[A], state: Logic.State, acc: Vector[A], stack: List[(Logic.State, Logic[A])]): Vector[A] =
      logic match {
        case Logic.Success(a) =>
          stack match {
            case Nil => acc :+ a
            case (state, h) :: t => loop(h, state, acc :+ a, t)
          }
        case Logic.Failure =>
          stack match {
            case Nil => acc
            case (state, h) :: t => loop(h, state, acc, t)
          }
        case Logic.FlatMap(Logic.Choose, k) =>
          loop(k(true), state, acc, (state, k(false)) :: stack)
        case Logic.FlatMap(Logic.Get, k) =>
          loop(k(state), state, acc, stack)
        case Logic.FlatMap(state: Logic.State, k) =>
          loop(k(()), state, acc, stack)
      }
    loop(this, Logic.State.empty, Vector.empty, Nil)
  }

  def toStream: Stream[A] = {
    def loop(logic: Logic[A], state: Logic.State): Stream[A] =
      logic match {
        case Logic.Success(a) => Stream(a)
        case Logic.Failure => Stream.empty
        case Logic.FlatMap(Logic.Get, k) => loop(k(state), state)
        case Logic.FlatMap(state: Logic.State, k) => loop(k(()), state)
        case Logic.FlatMap(Logic.Choose, k) => loop(k(true), state) #::: loop(k(false), state)
      }
    loop(this, Logic.State.empty)
  }
}

object Logic {
  case object Failure extends Logic[Nothing]

  case class Success[A](value: A) extends Logic[A]

  case class FlatMap[A, B](effect: Effect[A], k: Arrs[A, B]) extends Logic[B]

  sealed abstract class Effect[+A]

  case object Get extends Effect[State]

  case class State(id: Int, vars: IntMap[LVar[Any]]) extends Effect[Unit]

  object State {
    val empty = State(0, IntMap.empty)
  }

  case object Choose extends Effect[Boolean]

  val unit: Logic[Unit] = Success(())

  val get: Logic[State] = FlatMap(Get, Arrs((s: State) => Success(s)))

  def put(state: State): Logic[Unit] = FlatMap(state, Arrs((_: Unit) => unit))

  def choose[A](x: => Logic[A], y: => Logic[A]): Logic[A] = FlatMap(Choose, Arrs((test: Boolean) => if (test) x else y))

  def sum[A](list: List[Logic[A]]): Logic[A] = list.foldRight(Failure: Logic[A])(_ ||| _)

  def apply[A](xs: A*): Logic[A] = xs.foldRight(Failure: Logic[A])(Success(_) ||| _)
}
