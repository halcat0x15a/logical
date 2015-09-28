package logical

import scala.util.control.TailCalls._

sealed trait Logic[+A] { self =>

  final def run: Stream[A] = run(Env.empty)

  final def run[B >: A](env: Env): Stream[B] = {
    val (e, result) = Logic.resume(this, env, Stream.empty)
    result match {
      case Some((value, logs)) => value #:: logs.flatMap(_.run(e))
      case None => Stream.empty
    }
  }

  def map[B](f: A => B): Logic[B] = flatMap(a => Logic.success(f(a)))

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    this match {
      case FlatMap(self, g) => FlatMap(self, (x: Any) => FlatMap(g(x), f))
      case _ => FlatMap(this, f)
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap(_ => that)

  def |||[B >: A](that: => Logic[B]): Logic[B] =
    this match {
      case Append(self, f) => Append(self, () => Append(f(), () => that))
      case _ => Append(this, () => that)
    }

}

case object Failure extends Logic[Nothing]

case class Get[A](apply: Env => A) extends Logic[A]

case class Put[A](env: Env, value: A) extends Logic[A]

case class FlatMap[A, B](self: Logic[A], f: A => Logic[B]) extends Logic[B]

case class Append[A](self: Logic[A], f: () => Logic[A]) extends Logic[A]

case class Cut[A](self: Logic[A]) extends Logic[A]

object Logic {

  @annotation.tailrec
  final def resume[A](log: Logic[A], env: Env, acc: Stream[Logic[A]]): (Env, Option[(A, Stream[Logic[A]])]) =
    log match {
      case Failure =>
        if (acc.isEmpty)
          (env, None)
        else
          resume(acc.head, env, acc.tail)
      case Get(g) => (env, Some((g(env), acc)))
      case Put(env, value) => (env, Some((value, acc)))
      case Append(self, f) =>
        self match {
          case Failure => resume(f(), env, acc)
          case _ => resume(self, env, f() #:: acc)
        }
      case FlatMap(self, f) =>
        self match {
          case Failure => resume(Failure, env, acc)
          case Get(g) => resume(f(g(env)), env, acc)
          case Put(env, value) => resume(f(value), env, acc)
          case Append(self, g) => resume(self.flatMap(f), env, g().flatMap(f) #:: acc)
          case FlatMap(self, g) => resume(self.flatMap(g(_).flatMap(f)), env, acc)
        }
    }

  val get: Logic[Env] = Get(state => state)

  def success[A](value: A): Logic[A] = get.flatMap(Put(_, value))

  val True: Logic[Unit] = success(())

  implicit val monad: kits.Monad[Logic] =
    new kits.Monad[Logic] {
      def pure[A](a: A): Logic[A] = success(a)
      def flatMap[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = fa.flatMap(f)
    }

}
