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

}

object Cons {

  private case class Empty[A]() extends Cons[A]

  private case class Cell[A](head: Var[A], tail: Var[Cons[A]]) extends Cons[A]

  def apply[A](values: Var[A]*): Cons[A] =
    values.foldRight(Empty(): Cons[A])((value, acc) => Cell(value, Var(acc)))

  def fromList[A](list: List[A]): Cons[A] = apply(list.map(Var(_)): _*)

  def toList[A: Unify](cons: Var[Cons[A]]): Logic[Env, List[A]] = {
    val head = Var[A]
    val tail = Var[Cons[A]]
    cons === Var(Empty()) &&& Logic.succeed(List.empty[A]) ||| cons === Var(Cell(head, tail)) &&& toList(tail).flatMap(t => head.get.map(h => h :: t))
  }

  def append[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]], zs: Var[Cons[A]]): Logic[Env, Unit] = {
    val xh, zh = Var[A]
    val xt, zt = Var[Cons[A]]
    xs === Var(Empty[A]) &&& ys === zs ||| xs === Var(Cell(xh, xt)) &&& zs === Var(Cell(zh, zt)) &&& xh === zh &&& append(xt, ys, zt)
  }

  def size[A: Unify](cons: Var[Cons[A]], n: Var[Nat]): Logic[Env, Unit] = {
    val t = Var[Cons[A]]
    val m = Var[Nat]
    cons === Var(Empty[A]) &&& n === Var(Nat()) ||| cons === Var(Cell(Var[A], t)) &&& n === Var(Nat(m)) &&& size(t, m)
  }

  def contains[A: Unify](x: Var[A], xs: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t = Var[Cons[A]]
    xs === Var(Cell(h, t)) &&& (x === h ||| contains(x, t))
  }

  def permutations[A: Unify](xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val t = Var[Cons[A]]
    val zs = Var[Cons[A]]
    xs === Var(Empty[A]) &&& ys === Var(Empty[A]) ||| ys === Var(Cell(h, t)) &&& select(h, xs, zs) &&& permutations(zs, t)
  }

  def select[A: Unify](x: Var[A], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val xt = Var[Cons[A]]
    val yt = Var[Cons[A]]
    xs === Var(Cell(x, ys)) ||| xs === Var(Cell(h, xt)) &&& ys === Var(Cell(h, yt)) &&& select(x, xt, yt)
  }

  def combinations[A: Unify](n: Var[Nat], xs: Var[Cons[A]], ys: Var[Cons[A]]): Logic[Env, Unit] = {
    val h = Var[A]
    val xt = Var[Cons[A]]
    val yt = Var[Cons[A]]
    val m = Var[Nat]
    n === Var(Nat()) &&& ys === Var(Empty[A]) ||| n === Var(Nat(m)) &&& xs === Var(Cell(h, xt)) &&& (ys === Var(Cell(h, yt)) &&& combinations(m, xt, yt) ||| combinations(n, xt, ys))
  }

  implicit def unify[A: Unify]: Unify[Cons[A]] =
    new Unify[Cons[A]] {
      def unify(x: Cons[A], y: Cons[A]): Logic[Env, Unit] = x === y
    }

}
