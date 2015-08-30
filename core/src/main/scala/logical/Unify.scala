package logical

trait Unify[A] {

  def unify(x: A, y: A): Logic[Env, Unit]

}

object Unify {

  implicit def unify[A]: Unify[A] =
    new Unify[A] {
      def unify(x: A, y: A): Logic[Env, Unit] =
        if (x == y)
          Logic.succeed(())
        else
          Logic.fail
    }

}
