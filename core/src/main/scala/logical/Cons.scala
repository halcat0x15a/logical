package logical

sealed abstract class Cons[+A] {

  def ===[B >: A: Unify](that: Cons[B]): Logic[Unit] =
    (this, that) match {
      case (Empty, Empty) => True
      case (Cell(_, _), Empty) => Failure
      case (Empty, Cell(_, _)) => Failure
      case (Cell(x, xs), Cell(y, ys)) => x === y &&& xs === ys
    }

  def toList[B >: A: Unify]: Logic[List[A]] =
    this match {
      case Empty => Success(Nil)
      case Cell(head, tail) =>
        for {
          x <- head.get
          cons <- tail.get
          xs <- cons.toList[B]
        } yield x :: xs
    }

}

case object Empty extends Cons[Nothing]

case class Cell[A](head: Var[A], tail: Var[Cons[A]]) extends Cons[A]

object Cons {

  def apply[A]: Cons[A] = Empty

  def apply[A](head: Var[A], tail: Var[Cons[A]]): Cons[A] = Cell(head, tail)

  def apply[A](values: List[Var[A]]): Cons[A] =
    values.foldRight(Empty: Cons[A])((value, acc) => Cell(value, acc))

  def append[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]], zs: Var[Cons[A]]): Logic[Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Empty &&& ys === zs ||| xs === Cell(h, t) &&& zs === Cell(h, r) &&& append(t, ys, r)
  }

  def size[A: Unify](xs: Var[Cons[A]], n: Var[Nat]): Logic[Unit] = {
    val t = Var[Cons[A]]
    val m = Var[Nat]
    xs === Empty &&& n === Zero ||| xs === Cell(Var[A], t) &&& n === Succ(m) &&& size(t, m)
  }

  def contains[A: Unify](x: Var[A], xs: Var[Cons[A]]): Logic[Unit] = {
    val h = Var[A]
    val t = Var[Cons[A]]
    xs === Cell(h, t) &&& (x === h ||| contains(x, t))
  }

  def permutations[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Empty &&& ys === Empty ||| ys === Cell(h, t) &&& select(h, xs, r) &&& permutations(r, t)
  }

  def select[A: Unify](x: Var[A], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    xs === Cell(x, ys) ||| xs === Cell(h, t) &&& ys === Cell(h, r) &&& select(x, t, r)
  }

  def combinations[A: Unify](n: Var[Nat], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Unit] = {
    val h = Var[A]
    val t, r = Var[Cons[A]]
    val m = Var[Nat]
    n === Zero &&& ys === Empty ||| n === Succ(m) &&& xs === Cell(h, t) &&& (ys === Cell(h, r) &&& combinations(m, t, r) ||| combinations(n, t, ys))
  }

  implicit def unify[A: Unify]: Unify[Cons[A]] =
    new Unify[Cons[A]] {
      def unify(x: Cons[A], y: Cons[A]): Logic[Unit] = x === y
    }

}
