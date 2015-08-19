package logical

trait Logic[A] { self =>

  def apply(env: Env): Stream[(Env, A)]

  def run: Stream[A] =
    apply(Env(Set.empty, Map.empty)).map { case (_, value) => value }

  def map[B](f: A => B): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).map { case (env, a) => (env, f(a)) }
    }

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).flatMap { case (env, a) => f(a).apply(env) }
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap[B](_ => that)

  def |||(that: => Logic[A]): Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] =
        self.apply(env).append(that.apply(env))
    }

}

object Logic {

  def succeed[A](value: A): Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] = Stream((env, value))
    }

  def fail[A]: Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] = Stream.empty
    }

}
