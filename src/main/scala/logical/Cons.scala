package logical

sealed trait Cons[A] {

  import Cons._

  def ===(that: Cons[A])(implicit A: Unify[A]): Logic[Env, Unit] =
    (this, that) match {
      case (Empty(), Empty()) => Logic.succeed(())
      case (Cell(_, _), Empty()) => Logic.fail
      case (Empty(), Cell(_, _)) => Logic.fail
      case (Cell(x, xs), Cell(y, ys)) => x === y &&& xs === ys
    }

  def toList(implicit A: Unify[A]): Logic[Env, List[A]] =
    this match {
      case Empty() => Logic.succeed(List.empty)
      case Cell(head, tail) =>
        for {
          head <- head.get
          cons <- tail.get
          tail <- cons.toList
        } yield head :: tail
    }

}

object Cons {

  private case class Empty[A]() extends Cons[A]

  private case class Cell[A](head: Var[A], tail: Var[Cons[A]]) extends Cons[A]

  def apply[A](values: A*): Cons[A] =
    values.foldRight(Empty(): Cons[A])((value, acc) => Cell(Var(value), Var(acc)))

  def empty[A]: Cons[A] = Empty[A]

  def cell[A](head: Var[A], tail: Var[Cons[A]]): Cons[A] = Cell(head, tail)

  def append[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]], zs: Var[Cons[A]]): Logic[Env, Unit] = {
    val xh, zh = Var[A]
    val xt, zt = Var[Cons[A]]
    (xs === Var(empty) &&& ys === zs) ||| (xs === Var(cell(xh, xt)) &&& zs === Var(cell(zh, zt)) &&& xh === zh &&& append(xt, ys, zt))
  }

  implicit def unify[A: Unify]: Unify[Cons[A]] =
    new Unify[Cons[A]] {
      def unify(x: Cons[A], y: Cons[A]): Logic[Env, Unit] = x === y
    }

}
