package logical

trait Logic[S, +A] { self =>

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

  def |||[B >: A](that: => Logic[S, B]): Logic[S, B] =
    new Logic[S, B] {
      def apply(s: S): Stream[(S, B)] = self.apply(s).append(that.apply(s))
    }

}

object Logic {

  private case class Cut[S, A](self: Logic[S, A]) extends Logic[S, A] {

    def apply(s: S): Stream[(S, A)] = self.apply(s)

    override def map[B](f: A => B): Logic[S, B] = Cut(self.map(f))

    override def flatMap[B](f: A => Logic[S, B]): Logic[S, B] = Cut(self.flatMap(f))

    override def |||[B >: A](that: => Logic[S, B]): Logic[S, B] = {
      val logic = new Logic[S, B] {
        def apply(s: S): Stream[(S, B)] = {
          val r = self.apply(s)
          if (r.isEmpty) that.apply(s) else r
        }
      }
      Cut(logic)
    }

  }

  def cut[S, A](logic: Logic[S, A]): Logic[S, A] = Cut(logic)

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

  implicit def monad[S]: kits.Monad[({ type F[A] = Logic[S, A] })#F] =
    new kits.Monad[({ type F[A] = Logic[S, A] })#F] {
      def pure[A](a: A): Logic[S, A] = succeed(a)
      def flatMap[A, B](fa: Logic[S,A])(f: A => Logic[S,B]): Logic[S,B] = fa.flatMap(f)
    }

}
