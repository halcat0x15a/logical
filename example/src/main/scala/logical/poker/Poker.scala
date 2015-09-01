package logical

package poker

import scala.util.parsing.combinator.RegexParsers

object Poker extends App with RegexParsers {

  val suit = 'H' ^^^ Var(Heart) | 'D' ^^^ Var(Diamond) | 'C' ^^^ Var(Club) | 'S' ^^^ Var(Spade) | '?' ^^ (_ => Var[Suit])
  val number = """\d+""".r ^^ (n => Var(Nat(n.toInt))) | '?' ^^ (_ => Var[Nat])
  val card = suit ~ number ^^ { case s ~ n => Card(s, n) }

  parseAll(repN(5, card), args(0)) match {
    case Success(result, _) =>
      for (cards <- result.permutations.map(Cons(_))) {
        val hand = Var[Hand]
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
          } yield s"""$hand,${strs.mkString(",")}"""
        val app = Card.hand(cards) &&& (straightFlush ||| fourOfAKind ||| fullHouse ||| flush ||| straight ||| threeOfAKind ||| twoPairs ||| onePair) &&& show
        app.run.foreach(println)
      }
    case failure: NoSuccess =>
      sys.error(failure.msg)
  }

}
