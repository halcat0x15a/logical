package logical

sealed abstract class LList[+A] {
  def ===[B >: A](that: LList[B])(implicit unify: Unify[B]): Logic[Unit] =
    (this, that) match {
      case (LNil, LNil) =>
        Logic.unit
      case (LCons(x, xs), LCons(y, ys)) =>
        x === y &&& xs === ys
      case _ =>
        Logic.Failure
    }

  def toList: Logic[List[A]] =
    this match {
      case LNil =>
        Logic.Success(Nil)
      case LCons(h, t) =>
        for {
          h <- h.get
          t <- t.get
          t <- t.toList
        } yield h :: t
    }
}

case class LCons[A](head: LVar[A], tail: LVar[LList[A]]) extends LList[A]

case object LNil extends LList[Nothing]

object LList {
  def apply[A](values: A*): LList[A] =
    values.foldRight(LNil: LList[A])(LCons(_, _))

  def append[A: Unify](xs: LVar[LList[A]], ys: LVar[LList[A]], zs: LVar[LList[A]]): Logic[Unit] =
    xs === LNil &&& ys === zs ||| (for {
      h <- LVar[A]
      xt <- LVar[LList[A]]
      zt <- LVar[LList[A]]
      _ <- xs === LCons(h, xt) &&& zs === LCons(h, zt) &&& append(xt, ys, zt)
    } yield ())

  implicit class LVarOps[A](val self: LVar[LList[A]]) extends AnyVal {
    def toList: Logic[List[A]] = self.get.flatMap(_.toList)
  }

  implicit def unify[A](implicit unify: Unify[A]): Unify[LList[A]] =
    new Unify[LList[A]] {
      def apply(xs: LList[A], ys: LList[A]): Logic[Unit] = xs === ys
    }
}
