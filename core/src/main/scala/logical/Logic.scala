package logical

import scala.util.control.TailCalls._

sealed trait Logic[+A] { self =>

  final def run: Stream[A] = run(Env.empty).result

  final def run(env: Env): TailRec[Stream[A]] =
    this match {
      case Failure => done(Stream.empty)
      case Get(f) => done(Stream(f(env)))
      case Put(env, value) => done(Stream(value))
      case Or(self, f) => 
        for {
          x <- tailcall(self.run(env))
          y <- tailcall(f().run(env))
        } yield x.append(y)
      case FlatMap(self, f) =>
        self match {
          case Failure => done(Stream.empty)
          case Get(g) => tailcall(f(g(env)).run(env))
          case Put(env, value) => tailcall(f(value).run(env))
          case Or(self, g) => tailcall((self.flatMap(f) ||| g().flatMap(f)).run(env))
          case FlatMap(self, g) => tailcall(self.flatMap(g(_).flatMap(f)).run(env))
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
      case Or(self, f) => Or(self, () => Or(f(), () => that))
      case _ => Or(this, () => that)
    }

}

case class FlatMap[A, B](self: Logic[A], f: A => Logic[B]) extends Logic[B]

case class Get[A](apply: Env => A) extends Logic[A]

case class Put[A](env: Env, value: A) extends Logic[A]

case class Or[A](self: Logic[A], f: () => Logic[A]) extends Logic[A]

case class Cut[A](self: Logic[A]) extends Logic[A]

case object Failure extends Logic[Nothing]

object Logic {

  val get: Logic[Env] = Get(state => state)

  def success[A](value: A): Logic[A] = get.flatMap(Put(_, value))

  val True: Logic[Unit] = success(())

  implicit val monad: kits.Monad[Logic] =
    new kits.Monad[Logic] {
      def pure[A](a: A): Logic[A] = success(a)
      def flatMap[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = fa.flatMap(f)
    }

}
