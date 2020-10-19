package adt
// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.

// Attributions and useful links:
// https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
// https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
// https://en.wikipedia.org/wiki/Algebraic_data_type
object adt {
  sealed trait Suit
  object Suit {
    final case object Clubs extends Suit
    final case object Spades extends Suit
    final case object Hearts extends Suit
    final case object Diamonds extends Suit
    def create (suit: Char): Option[Suit] = suit.toLower match {
      case 'c'  => Some(Clubs)
      case 's'  => Some(Spades)
      case 'h'  => Some(Hearts)
      case 'd'  => Some(Diamonds)
      case _    => None
    }
  }

  final case class Rank private (value: Int) extends AnyVal
  object Rank {
    val ranksDict = Map(
      '2' -> 2,
      '3' -> 3,
      '4' -> 4,
      '5' -> 5,
      '6' -> 6,
      '7' -> 7,
      '8' -> 8,
      '9' -> 9,
      'T' -> 10,
      'J' -> 11,
      'Q' -> 12,
      'K' -> 13,
      'A' -> 14,
    )
    def create(rank: Char): Option[Rank] = rank.toUpper match {
      case x  if ranksDict.contains(x)  =>  Some(Rank(ranksDict.getOrElse(x, 0)))
      case _                            =>  None
    }
  }

  final case class Card (rank: Rank, suit: Suit)

  final case class Hand (cards: Set[Card])
  object Hand {
    def create(cards: Set[Card], omaha: Boolean): Option[Hand] = cards match {
      case x  if x.size == 2 && !omaha || x.size == 4 && omaha  => Some(Hand(x))
      case _                                                    => None
    }
  }

  final case class Board private (cards: Set[Card])
  object Board {
    def create (cards: Set[Card]): Option[Board] = cards match {
      case x  if x.size == 5  => Some(Board(x))
      case _                  => None
    }
  }

  sealed trait PokerCombination {
    def value: Int
  }
  object PokerCombination {
    final case class StraightFlush(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 9
    }
    final case class FourOfKind(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 8
    }
    final case class FullHouse(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 7
    }
    final case class Flush(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 6
    }
    final case class Straight(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 5
    }
    final case class ThreeOfKind(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 4
    }
    final case class TwoPair(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 3
    }
    final case class OnePair(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 2
    }
    final case class HighCard(combination: Set[Card]) extends PokerCombination {
      override def value: Int = 1
    }
  }

  final case class TestCase (board: Board, players: Set[Hand])
  final case class TestResult (result: Set[Hand])
}
