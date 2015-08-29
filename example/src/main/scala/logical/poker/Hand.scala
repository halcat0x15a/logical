package logical

package poker

sealed trait Hand
case object RoyalFlush extends Hand
case object StraightFlush extends Hand
case object FourOfAKind extends Hand
case object FullHouse extends Hand
case object Flush extends Hand
case object Straight extends Hand
case object ThreeOfAKind extends Hand
case object TwoPairs extends Hand
case object OnePair extends Hand
case object NoPair extends Hand

object Hand {

  implicit val unify: Unify[Hand] = Unify.unify

}
