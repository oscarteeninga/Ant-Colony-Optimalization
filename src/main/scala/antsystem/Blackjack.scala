package antsystem

import antsystem.Deck.Color

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.language.implicitConversions
import scala.util.{Random, Try}

abstract case class Card(value: Int)

object Card {

  object Two extends Card(2)

  object Three extends Card(3)

  object Four extends Card(4)

  object Five extends Card(5)

  object Six extends Card(6)

  object Seven extends Card(7)

  object Eight extends Card(8)

  object Nine extends Card(9)

  object Ten extends Card(10)

  object Jack extends Card(10)

  object Queen extends Card(10)

  object King extends Card(10)

  object As extends Card(11)

  def fromString(str: String): Card =
    Color.find(_.value == str.toInt).getOrElse(throw new Exception("Card not found"))
}

case class Deck(cards: List[Card]) {

  def drop(card: Card): Deck = {
    val index = cards.indexOf(card)
    Deck(cards.take(index) ++ cards.drop(index + 1))
  }

  def getRandomAndDrop: (Card, Deck) = {
    val index = Math.abs(Random.nextInt()) % cards.size
    (cards(index), Deck(cards.take(index) ++ cards.drop(index + 1)))
  }
}

object Deck {

  import Card._

  val Color: List[Card] = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, As)
  private val All: List[Card] = (1 to 4).flatMap(_ => Color).toList

  val Full: Deck = Deck(All)
}

case class Player(point: List[Int]) {

  import Card._

  def put(card: Card): Player = {
    val points = if (card == As) List(1, 11).flatMap(v => point.map(_ + v)) else point.map(_ + card.value)
    Player(points.filter(_ < 22))
  }

  def enough: Boolean = point.exists(_ > 16)

  def loose: Boolean = point.isEmpty

  def winWith(other: Player): Boolean = point.max > other.point.max
}

object Player {
  val New: Player = Player(List(0))
}

case class Dealer(p1: Card, p2: Card, d: Card) {

  def game: Results = {
    def play(player: Player, dealer: Player, deck: Deck): Results = {
      if (dealer.loose) Results.Player
      else if (player.loose) Results.Dealer
      else if (!player.enough) deck.cards.map(card => play(player.put(card), dealer, deck.drop(card)))
      else if (!dealer.enough) deck.cards.map(card => play(player, dealer.put(card), deck.drop(card)))
      else Results(player, dealer)
    }

    play(Player.New.put(p1).put(p2), Player.New.put(d), Deck.Full.drop(p1).drop(p2).drop(d))
  }

  def probabilityGame(tries: Int): Results = {
    @tailrec
    def play(player: Player, dealer: Player, deck: Deck): Results = {
      if (dealer.loose) Results.Player
      else if (player.loose) Results.Dealer
      else if (!player.enough) {
        val (card, newDeck) = deck.getRandomAndDrop
        play(player.put(card), dealer.copy(), newDeck)
      } else if (!dealer.enough) {
        val (card, newDeck) = deck.getRandomAndDrop
        play(player.copy(), dealer.put(card), newDeck)
      } else Results(player, dealer)
    }

    (1 to tries).toList.map(_ => play(Player.New.put(p1).put(p2), Player.New.put(d), Deck.Full.drop(p1).drop(p2).drop(d)))
  }
}

case class Results(wins: Long, draws: Long, loose: Long) {

  def +(other: Results): Results = Results(wins + other.wins, draws + other.draws, loose + other.loose)

  override def toString: String = {
    val all = wins + draws + loose

    def proc(value: Long): String = (((value.toDouble / all) * 10000).round.toDouble / 100).toString

    s"Player: ${proc(wins)} %\nDraw: ${proc(draws)} %\nDealer: ${proc(loose)} %"
  }
}

object Results {

  val Player: Results = Results(1, 0, 0)
  val Dealer: Results = Results(0, 0, 1)
  private val Draw: Results = Results(0, 1, 0)

  def apply(player: Player, dealer: Player): Results =
    if (player.winWith(dealer)) Player else if (dealer.winWith(player)) Dealer else Draw

  implicit def reduce(results: List[Results]): Results = results.reduce(_ + _)
}

object Game extends App {
  while (true) {
    Try {
      print("\nPlayer card: ")
      val player1 = readLine()
      print("Dealer card: ")
      val dealer = readLine()
      print("Player card: ")
      val player2 = readLine()
      val game = Dealer(Card.fromString(player1), Card.fromString(player2), Card.fromString(dealer))
//      println("\n=== BRUTE FORCE ===\n" + game.game)
      println("\n=== MONTE CARLO ===\n" + game.probabilityGame(Int.MaxValue / 10000))
    }
  }
}
