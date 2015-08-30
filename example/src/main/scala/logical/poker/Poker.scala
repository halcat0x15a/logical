package logical

package poker

object Poker extends App {

  val card = """([HDCS?])(\d+|\?)""".r

  val input = card.findAllMatchIn(args(0)).map {
    case card(s, n) =>
      val suit: Var[Suit] =
        s match {
          case "H" => Heart
          case "D" => Diamond
          case "C" => Club
          case "S" => Spade
          case _ => Var[Suit]
        }
      val number: Var[Nat] =
        if (n == "?")
          Var[Nat]
        else
          Nat(n.toInt)
      Card(suit, number)
  }

  val hand = Var[Hand]
  val cards = Cons(input.toList)

  val onePair = Logic.cut(hand === OnePair &&& Card.onePair(cards))
  val twoPairs = Logic.cut(hand === TwoPairs &&& Card.twoPairs(cards))
  val threeOfAKind = Logic.cut(hand === ThreeOfAKind &&& Card.threeOfAKind(cards))
  val straight = Logic.cut(hand === Straight &&& Card.straight(cards))
  val flush = Logic.cut(hand === Flush &&& Card.flush(cards))
  val fullHouse = Logic.cut(hand === FullHouse &&& Card.fullHouse(cards))
  val fourOfAKind = Logic.cut(hand === FourOfAKind &&& Card.fourOfAKind(cards))
  val straightFlush = Logic.cut(hand === StraightFlush &&& Card.straightFlush(cards))

  val output =
    for {
      h <- hand.get
      cs <- cards.get
      l <- cs.toList
      ss <- kits.Traverse.traverse[List, ({ type F[A] = Logic[Env, A] })#F, Card, String](l) { c =>
        for {
          s <- c.suit.get
          n <- c.number.get
          i <- n.toInt
        } yield s"${s.toString.head}$i"
      }
    } yield s"""$h,${ss.mkString}"""
  
  val app = Card.hand(cards) &&& (straightFlush ||| fourOfAKind ||| fullHouse ||| flush ||| straight ||| threeOfAKind ||| twoPairs ||| onePair) &&& output

  app.run.foreach(println)

}
