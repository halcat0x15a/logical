package logical

package fizzbuzz

object FizzBuzz extends App {

  val num = Var[Nat]
  val str = Var[String]

  val fizz = Logic.cut(str === "Fizz" &&& Nat.divmod(num, Nat(3), Nat(Var[Nat]), Nat()))
  val buzz = Logic.cut(str === "Buzz" &&& Nat.divmod(num, Nat(5), Nat(Var[Nat]), Nat()))
  val fizzbuzz = Logic.cut(str === "FizzBuzz" &&& Nat.divmod(num, Nat(15), Nat(Var[Nat]), Nat()))

  val app = Nat.nat(num) &&& ((fizzbuzz ||| fizz ||| buzz) &&& str.get ||| num.get.flatMap(_.toInt).map(_.toString))

  app.run.take(100).foreach(println)

}
