package logical

trait Unify[A] {

  def unify(x: A, y: A): Logic[Unit]

}

object Unify {

  def unify[A]: Unify[A] =
    new Unify[A] {
      def unify(x: A, y: A): Logic[Unit] =
        if (x == y)
          Logic.succeed(())
        else
          Logic.fail
    }

  implicit def int: Unify[Int] = unify

  implicit def list[A]: Unify[List[A]] = unify

}
