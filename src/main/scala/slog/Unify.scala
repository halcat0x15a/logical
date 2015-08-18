package slog

trait Unify[A] {

  def unify(x: A, y: A): Logic

}

object Unify {

  def unify[A]: Unify[A] =
    new Unify[A] {
      def unify(x: A, y: A): Logic =
        if (x == y) Logic.succeed else Logic.fail
    }

  implicit def int: Unify[Int] = unify

  implicit def list[A]: Unify[List[A]] = unify

}
