package logical

trait Logic[+A] { self =>

  def apply(env: Env): Stream[(Env, A)]

  def run: Stream[A] = apply(Env.empty).map { case (_, value) => value }

  def map[B](f: A => B): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).map { case (env, value) => (env, f(value)) }
    }

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).flatMap { case (env, value) => f(value).apply(env) }
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap(_ => that)

  def |||[B >: A](that: => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] = self.apply(env).append(that.apply(env))
    }

}

case class Cut[A](self: Logic[A]) extends Logic[A] {

  def apply(env: Env): Stream[(Env, A)] = self.apply(env)

  override def map[B](f: A => B): Logic[B] = Cut(self.map(f))

  override def flatMap[B](f: A => Logic[B]): Logic[B] = Cut(self.flatMap(f))

  override def |||[B >: A](that: => Logic[B]): Logic[B] =
    Cut(new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] = {
        val result = self.apply(env)
        if (result.isEmpty) that.apply(env) else result
      }
    })

}

case class Success[A](value: A) extends Logic[A] {

  def apply(env: Env): Stream[(Env, A)] = Stream((env, value))

}

case object True extends Logic[Unit] {

  def apply(env: Env): Stream[(Env, Unit)] = Stream((env, ()))

}

case object Failure extends Logic[Nothing] {

  def apply(env: Env): Stream[(Env, Nothing)] = Stream.empty

}

object Logic {

  implicit val monad: kits.Monad[Logic] =
    new kits.Monad[Logic] {
      def pure[A](a: A): Logic[A] = Success(a)
      def flatMap[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = fa.flatMap(f)
    }

}
