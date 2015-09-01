package logical

trait Logic[+A] { self =>

  def apply(env: Env): Stream[(Env, A)]

  def run(implicit env: Env): Stream[A] = apply(env).map(_._2)

  def map[B](f: A => B): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).map(pair => pair.copy(_2 = f(pair._2)))
    }

  def flatMap[B](f: A => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] =
        self.apply(env).flatMap(pair => f(pair._2).apply(pair._1))
    }

  def &&&[B](that: => Logic[B]): Logic[B] = flatMap(_ => that)

  def |||[B >: A](that: => Logic[B]): Logic[B] =
    new Logic[B] {
      def apply(env: Env): Stream[(Env, B)] = self.apply(env).append(that.apply(env))
    }

}

object Logic {

  private case class Cut[A](self: Logic[A]) extends Logic[A] {

    def apply(env: Env): Stream[(Env, A)] = self.apply(env)

    override def map[B](f: A => B): Logic[B] = Cut(self.map(f))

    override def flatMap[B](f: A => Logic[B]): Logic[B] = Cut(self.flatMap(f))

    override def |||[B >: A](that: => Logic[B]): Logic[B] = {
      val logic = new Logic[B] {
        def apply(env: Env): Stream[(Env, B)] = {
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
      def apply(env: Env): Stream[(Env, A)] = Stream((env, value))
    }

  def fail[A]: Logic[A] =
    new Logic[A] {
      def apply(env: Env): Stream[(Env, A)] = Stream.empty
    }

  implicit def monad[S]: kits.Monad[Logic] =
    new kits.Monad[({ type F[A] = Logic[A] })#F] {
      def pure[A](a: A): Logic[A] = succeed(a)
      def flatMap[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] = fa.flatMap(f)
    }

}
