package logical

trait Unify[-A] {

  def unify(x: A, y: A): Logic[Unit]

}

object Unify {

  implicit def unify[A]: Unify[A] =
    new Unify[A] {
      def unify(x: A, y: A): Logic[Unit] =
        if (x == y)
          True
        else
          Failure
    }

}
