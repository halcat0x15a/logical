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
      case Empty() => Logic.succeed(Nil)
      case Cell(head, tail) =>
        for {
          x <- head.get
          cons <- tail.get
          xs <- cons.toList
        } yield x :: xs
    }

}

object Cons {

  private case class Empty[A]() extends Cons[A]

  private case class Cell[A](head: Var[A], tail: Var[Cons[A]]) extends Cons[A]

  def apply[A]: Cons[A] = Empty[A]

  def apply[A](head: Var[A], tail: Var[Cons[A]]): Cons[A] = Cell(head, tail)

  def apply[A](values: List[Var[A]]): Cons[A] =
    values.foldRight(Empty(): Cons[A])((value, acc) => Cell(value, acc))

  def append[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]], zs: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Empty[A] &&& ys === zs ||| xs === Cell(h, t) &&& zs === Cell(h, r) &&& append(t, ys, r)
  }

  def size[A: Unify](xs: Var[Cons[A]], n: Var[Nat]): Logic[Env, Unit] = {
    val t = Var[Cons[A]]
    val m = Var[Nat]
    xs === Empty[A] &&& n === Nat() ||| xs === Cell(Var[A], t) &&& n === Nat(m) &&& size(t, m)
  }

  def contains[A: Unify](x: Var[A], xs: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t = Var[Cons[A]]
    xs === Cell(h, t) &&& (x === h ||| contains(x, t))
  }

  def permutations[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Empty[A] &&& ys === Empty[A] ||| ys === Cell(h, t) &&& select(h, xs, r) &&& permutations(r, t)
  }

  def select[A: Unify](x: Var[A], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Cell(x, ys) ||| xs === Cell(h, t) &&& ys === Cell(h, r) &&& select(x, t, r)
  }

  def combinations[A: Unify](n: Var[Nat], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    val m = Var[Nat]
    n === Nat() &&& ys === Empty[A] ||| n === Nat(m) &&& xs === Cell(h, t) &&& (ys === Cell(h, r) &&& combinations(m, t, r) ||| combinations(n, t, ys))
  }

  implicit def unify[A: Unify]: Unify[Cons[A]] =
    new Unify[Cons[A]] {
      def unify(x: Cons[A], y: Cons[A]): Logic[Env, Unit] = x === y
    }

}
