package logical

trait Unify[A] {

  def unify(x: A, y: A): Logic[Env, Unit]

}

object Unify {

  def unify[A]: Unify[A] =
    new Unify[A] {
      def unify(x: A, y: A): Logic[Env, Unit] =
        if (x == y)
          Logic.succeed(())
        else
          Logic.fail
    }

  implicit def int: Unify[Int] = unify

  implicit def string: Unify[String] = unify

  implicit def list[A]: Unify[List[A]] = unify

}
