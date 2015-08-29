package logical

package poker

object Poker {

  val card = """([HDCS?])(\d+|\?)""".r

  def hand(cards: Var[Cons[Card]], hand: Var[Hand]): Logic[Env, Unit] = {
    val onePair = hand === Var(OnePair) &&& Card.onePair(cards)
    val twoPairs = hand === Var(TwoPairs) &&& Card.twoPairs(cards)
    val threeOfAKind = hand === Var(ThreeOfAKind) &&& Card.threeOfAKind(cards)
    val straight = hand === Var(Straight) &&& Card.straight(cards)
    val flush = hand === Var(Flush) &&& Card.flush(cards)
    val fullHouse = hand === Var(FullHouse) &&& Card.fullHouse(cards)
    val fourOfAKind = hand === Var(FourOfAKind) &&& Card.fourOfAKind(cards)
    val straightFlush = hand === Var(StraightFlush) &&& Card.straightFlush(cards)
    Card.hand(cards) &&& (onePair/* ||| twoPairs ||| threeOfAKind ||| straight ||| flush ||| fullHouse ||| fourOfAKind ||| straightFlush*/)
  }

  def read(in: String): List[Card] =
    card.findAllMatchIn(in).map {
      case card(s, n) =>
        val suit = s match {
          case "H" => Var[Suit](Heart)
          case "D" => Var[Suit](Diamond)
          case "C" => Var[Suit](Club)
          case "S" => Var[Suit](Spade)
          case _ => Var[Suit]
        }
        val number = if (n == "?") Var[Nat] else Var(Nat(n.toInt))
        Card(suit, number)
    }.toList

  def write(out: Var[Cons[Card]]): Logic[Env, List[String]] =
    Cons.toList(out).flatMap(cards => Logic.sequence(cards.map(Card.toString)))

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      val input = args(0)
      val h = Var[Hand]
      val cards = Var(Cons.fromList(read(input)))
      (hand(cards, h) &&& (for (h <- h.get; cs <- write(cards)) yield (h, cs))).run.foreach(println)
    }
  }

}
