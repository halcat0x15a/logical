package logical

import org.scalatest.FunSuite

class Example extends FunSuite {

  test("hoge") {
    val xs = Var(Cons(1, 2, 3))
    val ys = Var(Cons(4, 5))
    val zs = Var[Cons[Int]]
    val ws = Var[Cons[Int]]
    val r = Var[List[Int]]
    val s = Var[List[Int]]
    val q = Var[List[List[Int]]]
    val a = Var(Cons(1, 2, 3, 4, 5))
    assert((Cons.append(xs, ys, zs) &&& zs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3, 4, 5)))
    assert((Cons.append(xs, zs, a) &&& zs.get.flatMap(_.toList)).run == Stream(List(4, 5)))
    //assert((Cons.append(ws, zs, a) &&& Cons.toList(ws, r) &&& Cons.toList(zs, s) &&& Cons.toList(Var(Cons.cell(r, Var(Cons.cell(s, Var(Cons.empty[List[Int]]))))), q)).run(q) == Stream(List(1, 2, 3, 4, 5)))
  }

  test("a") {
    val a = Var[Int]
    val b = Var[Int]
    val c = Var[Int]
    val xs = Var[Cons[Int]]
    val ys = Var[Cons[Int]]
    val zs = Var[Cons[Int]]
    assert((a === Var(0) &&& a.get).run == Stream(0))
    assert((Var(0) === a &&& a.get).run == Stream(0))
    assert(((a === Var(0) ||| a === Var(1)) &&& a.get).run == Stream(0, 1))
    assert((a === b &&& b === Var(0) &&& a.get).run == Stream(0))
    assert((a === b &&& b === c &&& c === Var(0) &&& a.get).run == Stream(0))
    assert((a === b &&& c === b &&& c === Var(0) &&& a.get).run == Stream(0))
    assert((Cons.cell(a, xs) === Cons.cell(Var(0), ys) &&& a.get).run == Stream(0))
    assert((Var(Cons.cell(a, xs)) === Var(Cons.cell(Var(0), ys)) &&& a.get).run == Stream(0))
    assert((xs === Var(Cons.cell(a, ys)) &&& xs === Var(Cons.cell(Var(0), ys)) &&& a.get).run == Stream(0))
    assert((xs === Var(Cons.cell(a, ys)) &&& xs === Var(Cons.empty) &&& a.get).run == Stream())
  }

}
