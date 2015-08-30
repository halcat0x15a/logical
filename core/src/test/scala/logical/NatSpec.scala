package logical

import org.scalatest.FunSpec

class NatSpec extends FunSpec {

  describe("Nat") {

    it("plus") {
      val x = Var[Nat]
      val y = Var[Nat]
      assert((Nat.plus(Var(Nat(2)), Var(Nat(3)), x) &&& Nat.toInt(x)).run == Stream(5))
      assert((Nat.plus(Var(Nat(2)), x, Var(Nat(5))) &&& Nat.toInt(x)).run == Stream(3))
      assert((Nat.plus(x, Var(Nat(2)), Var(Nat(5))) &&& Nat.toInt(x)).run == Stream(3))
      assert((Nat.plus(x, y, Var(Nat(3))) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run == Stream((0, 3), (1, 2), (2, 1), (3, 0)))
      assert((Nat.plus(x, Var(Nat(3)), y) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run.take(3) == Stream((0, 3), (1, 4), (2, 5)))
      assert((Nat.plus(Var(Nat(3)), x, y) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run.take(3) == Stream((0, 3), (1, 4), (2, 5)))
    }

    it("lteq") {
      val n = Var[Nat]
      assert(Nat.lteq(Var(Nat(2)), Var(Nat(3))).run == Stream(()))
      assert(Nat.lteq(Var(Nat(3)), Var(Nat(3))).run == Stream(()))
      assert(Nat.lteq(Var(Nat(4)), Var(Nat(3))).run == Stream())
      assert((Nat.lteq(n, Var(Nat(3))) &&& Nat.toInt(n)).run == Stream(0, 1, 2, 3))
      assert((Nat.lteq(Var(Nat(1)), n) &&& Nat.toInt(n)).run.take(3) == Stream(1, 2, 3))
    }

    it("mod") {
      val q = Var[Nat]
      val r = Var[Nat]
      assert((Nat.divmod(Var(Nat(1)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((0, 1)))
      assert((Nat.divmod(Var(Nat(2)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 0)))
      assert((Nat.divmod(Var(Nat(3)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 1)))
      assert((Nat.divmod(Var(Nat(5)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((2, 1)))
      assert((Nat.divmod(Var(Nat(5)), Var(Nat(3)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 2)))
      assert((Nat.divmod(q, Var(Nat(3)), Var(Nat(2)), r) &&& Nat.toInt(q)).run.take(1) == Stream(6))
    }

  }

}
