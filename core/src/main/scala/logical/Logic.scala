package logical

trait Logic[+A] { self =>

  def apply(env: Env): Stream[Env]

  def run(implicit env: Env): Stream[A] = apply(env).map(_.value.asInstanceOf[A])

  def map[B](f: A => B): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[Env] =
        self.apply(env).map(env => env.copy(f(env.value.asInstanceOf[A])))
    }

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[Env] =
        self.apply(env).flatMap(env => f(env.value.asInstanceOf[A]).apply(env))
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap(_ => that)

  def |||[B >: A](that: => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[Env] = self.apply(env).append(that.apply(env))
    }

}

object Logic {

  private case class Cut[A](self: Logic[A]) extends Logic[A] {

    def apply(env: Env): Stream[Env] = self.apply(env)

    override def map[B](f: A => B): Logic[B] = Cut(self.map(f))

    override def flatMap[B](f: A => Logic[B]): Logic[B] = Cut(self.flatMap(f))

    override def |||[B >: A](that: => Logic[B]): Logic[B] = {
      val logic = new Logic[B] {
        def apply(env: Env): Stream[Env] = {
          val r = self.apply(env)
          if (r.isEmpty) that.apply(env) else r
        }
      }
      Cut(logic)
    }

  }

  def cut[A](logic: Logic[A]): Logic[A] = Cut(logic)

  def succeed[A](value: A): Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[Env] = Stream(env.copy(value = value))
    }

  def fail[A]: Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[Env] = Stream.empty
    }

  implicit def monad[S]: kits.Monad[Logic] =
    new kits.Monad[({ type F[A] = Logic[A] })#F] {
      def pure[A](a: A): Logic[A] = succeed(a)
      def flatMap[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = fa.flatMap(f)
    }

}
