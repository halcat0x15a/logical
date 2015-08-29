package logical

trait Logic[S, A] { self =>

  def apply(s: S): Stream[(S, A)]

  def run(implicit s: S): Stream[A] =
    apply(s).map { case (_, value) => value }

  def map[B](f: A => B): Logic[S, B] =
    new Logic[S, B] {
      def apply(s: S): Stream[(S, B)] =
        self.apply(s).map { case (s, a) => (s, f(a)) }
    }

  def flatMap[B](f: A => Logic[S, B]): Logic[S, B] =
    new Logic[S, B] {
      def apply(s: S): Stream[(S, B)] =
        self.apply(s).flatMap { case (s, a) => f(a).apply(s) }
    }

  def unary_! : Logic[S, Unit] =
    new Logic[S, Unit] {
      def apply(s: S): Stream[(S, Unit)] =
        if (self.apply(s).isEmpty)
          Stream((s, ()))
        else
          Stream.empty
    }

  def &&&[B](that: => Logic[S, B]): Logic[S, B] = flatMap(_ => that)

  def |||(that: => Logic[S, A]): Logic[S, A] =
    new Logic[S, A] {
      def apply(s: S): Stream[(S, A)] = self.apply(s).append(that.apply(s))
    }

}

object Logic {

  def succeed[S, A](value: A): Logic[S, A] =
    new Logic[S, A] {
      def apply(s: S): Stream[(S, A)] = Stream((s, value))
    }

  def fail[S, A]: Logic[S, A] =
    new Logic[S, A] {
      def apply(s: S): Stream[(S, A)] = Stream.empty
    }

  def get[S]: Logic[S, S] =
    new Logic[S, S] {
      def apply(s: S): Stream[(S, S)] = Stream((s, s))
    }

  def put[S](state: S): Logic[S, Unit] =
    new Logic[S, Unit] {
      def apply(s: S): Stream[(S, Unit)] = Stream((state, ()))
    }

  def sequence[S, A](logs: List[Logic[S, A]]): Logic[S, List[A]] =
    logs.foldRight(succeed[S, List[A]](Nil))((log, acc) => for (as <- acc; a <- log) yield a :: as)

}
