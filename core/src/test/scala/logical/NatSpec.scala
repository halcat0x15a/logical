package logical

import org.scalatest.FunSpec

class NatSpec extends FunSpec {

  describe("Nat") {

    it("plus") {
      val x, y = Var[Nat]
      assert((Nat.plus(Nat(2), Nat(3), x) &&& x.get.flatMap(_.toInt)).run == Stream(5))
      assert((Nat.plus(Nat(2), x, Nat(5)) &&& x.get.flatMap(_.toInt)).run == Stream(3))
      assert((Nat.plus(x, Nat(2), Nat(5)) &&& x.get.flatMap(_.toInt)).run == Stream(3))
      assert((Nat.plus(x, y, Nat(3)) &&& (for (x <- x.get.flatMap(_.toInt); y <- y.get.flatMap(_.toInt)) yield (x, y))).run == Stream((0, 3), (1, 2), (2, 1), (3, 0)))
      assert((Nat.plus(x, Nat(3), y) &&& (for (x <- x.get.flatMap(_.toInt); y <- y.get.flatMap(_.toInt)) yield (x, y))).run.take(3) == Stream((0, 3), (1, 4), (2, 5)))
    }

    it("lteq") {
      val n = Var[Nat]
      assert(Nat.lteq(Nat(2), Nat(3)).run == Stream(()))
      assert(Nat.lteq(Nat(3), Nat(3)).run == Stream(()))
      assert(Nat.lteq(Nat(4), Nat(3)).run == Stream())
      assert((Nat.lteq(n, Nat(3)) &&& n.get.flatMap(_.toInt)).run == Stream(0, 1, 2, 3))
    }

    it("divmod") {
      val q, r = Var[Nat]
      assert((Nat.divmod(Nat(1), Nat(2), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((0, 1)))
      assert((Nat.divmod(Nat(2), Nat(2), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((1, 0)))
      assert((Nat.divmod(Nat(3), Nat(2), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((1, 1)))
      assert((Nat.divmod(q, Nat(3), Nat(2), r) &&& q.get.flatMap(_.toInt)).run.headOption == Some(6))
    }

  }

}
