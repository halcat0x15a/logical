package slog

sealed trait Cons[A] {

  import Cons._

  def ===(that: Cons[A])(implicit A: Unify[A]): Logic =
    (this, that) match {
      case (Empty(), Empty()) => Logic.succeed
      case (Cell(_, _), Empty()) => Logic.fail
      case (Empty(), Cell(_, _)) => Logic.fail
      case (Cell(x, xs), Cell(y, ys)) => x === y &&& xs === ys
    }

}

object Cons {

  private case class Empty[A]() extends Cons[A]

  private case class Cell[A](head: Var[A], tail: Var[Cons[A]]) extends Cons[A]

  def apply[A](values: A*): Cons[A] =
    values.foldRight(Empty(): Cons[A])((value, acc) => Cell(Var(value), Var(acc)))

  def empty[A]: Cons[A] = Empty[A]

  def cell[A](head: Var[A], tail: Var[Cons[A]]): Cons[A] = Cell(head, tail)

  def append[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]], zs: Var[Cons[A]]): Logic = {
    val xh, zh = Var[A]
    val xt, zt = Var[Cons[A]]
    (xs === Var(empty) &&& ys === zs) ||| (xs === Var(cell(xh, xt)) &&& zs === Var(cell(zh, zt)) &&& xh === zh &&& append(xt, ys, zt))
  }

  def toList[A: Unify](xs: Var[Cons[A]], ys: Unbound[List[A]]): Logic =
    new Logic {
      def apply(env: Env): Stream[Env] =
        Var.get(env, xs).flatMap(get(env, _)).toStream.flatMap(env.set(ys.id, _))
    }

  def get[A](env: Env, xs: Cons[A]): Option[List[A]] =
    xs match {
      case Empty() => Some(List.empty[A])
      case Cell(head, tail) =>
        for {
          head <- Var.get(env, head)
          cons <- Var.get(env, tail)
          tail <- get(env, cons)
        } yield head :: tail
    }

  implicit def unify[A: Unify]: Unify[Cons[A]] =
    new Unify[Cons[A]] {
      def unify(x: Cons[A], y: Cons[A]): Logic = x === y
    }

}
