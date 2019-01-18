package logical

trait Unify[A] {
  def apply(x: A, y: A): Logic[Unit]
}

object Unify extends LowPriorityUnify

trait LowPriorityUnify {
  implicit def unify[A]: Unify[A] =
    new Unify[A] {
      def apply(x: A, y: A): Logic[Unit] =
        if (x == y)
          Logic.unit
        else
          Logic.Failure
    }
}
