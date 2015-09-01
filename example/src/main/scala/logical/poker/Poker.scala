package logical

package poker

import scala.util.parsing.combinator.RegexParsers

object Poker extends App with RegexParsers {

  val suit = 'H' ^^^ Var(Heart) | 'D' ^^^ Var(Diamond) | 'C' ^^^ Var(Club) | 'S' ^^^ Var(Spade) | '?' ^^ (_ => Var[Suit])
  val number = """\d+""".r ^^ (n => Var(Nat(n.toInt))) | '?' ^^ (_ => Var[Nat])
  val card = suit ~ number ^^ { case s ~ n => Card(s, n) }

  def hand(cards: Cons[Card]): Logic[Hand] = {
    val hand = Var[Hand]
    val noPair = hand === NoPair
    val onePair = Logic.cut(hand === OnePair &&& Card.onePair(cards))
    val twoPairs = Logic.cut(hand === TwoPairs &&& Card.twoPairs(cards))
    val threeOfAKind = Logic.cut(hand === ThreeOfAKind &&& Card.threeOfAKind(cards))
    val straight = Logic.cut(hand === Straight &&& Card.straight(cards))
    val flush = Logic.cut(hand === Flush &&& Card.flush(cards))
    val fullHouse = Logic.cut(hand === FullHouse &&& Card.fullHouse(cards))
    val fourOfAKind = Logic.cut(hand === FourOfAKind &&& Card.fourOfAKind(cards))
    val straightFlush = Logic.cut(hand === StraightFlush &&& Card.straightFlush(cards))
    val show =
      for {
        hand <- hand.get
        cons <- cards.get
        cards <- cons.toList
        strs <- kits.Traverse.traverse(cards) { card =>
          for {
            suit <- card.suit.get
            num <- card.number.get
            int <- num.toInt
          } yield s"$suit($int)"
        }
      } yield {
        println(s"""$hand,${strs.mkString(",")}""")
        hand
      }
    Card.hand(cards) &&& (straightFlush ||| fourOfAKind ||| fullHouse ||| flush ||| straight ||| threeOfAKind ||| twoPairs ||| onePair ||| noPair) &&& show
  }

  parseAll(repN(5, card), args(0)) match {
    case Success(result, _) =>
      val hands = for {
        cards <- result.permutations.map(Cons(_)).toStream.par
        hand <- hand(cards).run
      } yield hand
      println(hands.groupBy(identity).mapValues(hs => (hs.size.toDouble / hands.size * 10000).toInt.toDouble / 100 + "%"))
    case failure: NoSuccess =>
      sys.error(failure.msg)
  }

}
