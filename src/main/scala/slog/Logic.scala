package slog

trait Logic { self =>

  def apply(env: Env): Stream[Env]

  def run[A](key: Var[A]): Stream[A] =
    apply(Env(Set.empty, Map.empty)).flatMap(Var.get(_, key))

  def &&&(that: => Logic): Logic =
    new Logic {
      def apply(env: Env): Stream[Env] =
        self.apply(env).flatMap(that.apply)
    }

  def |||(that: => Logic): Logic =
    new Logic {
      def apply(env: Env): Stream[Env] =
        self.apply(env).append(that.apply(env))
    }

}

object Logic {

  def succeed: Logic =
    new Logic {
      def apply(env: Env): Stream[Env] = Stream(env)
    }

  def fail: Logic =
    new Logic {
      def apply(env: Env): Stream[Env] = Stream.empty
    }

}
