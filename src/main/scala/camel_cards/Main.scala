package camel_cards

import scala.io.Source
import scala.util.matching.Regex

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/camel_cards/puzzle_day7.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    var pokerHands: Map[String, Array[(String, Int)]] = Map(
      "five_kind" -> Array.empty,
      "four_kind" -> Array.empty,
      "full_house" -> Array.empty,
      "three_kind" -> Array.empty,
      "two_pair" -> Array.empty,
      "one_pair" -> Array.empty,
      "high_card" -> Array.empty
    )

    val handOrder: Seq[String] = Seq(
      "high_card",
      "one_pair",
      "two_pair",
      "three_kind",
      "full_house",
      "four_kind",
      "five_kind"
    )

    val mapDict: Map[Char, Int] = Map(
      'A' -> 14,
      'K' -> 13,
      'Q' -> 12,
      'J' -> 1,
      'T' -> 10
    )

    def compare_card(card1: String, card2: String): Boolean = {
      for (i <- 0 to 4) {
        val _card1 = if card1.apply(i).isDigit then card1.apply(i).asDigit else mapDict(card1.apply(i))
        val _card2 = if card2.apply(i).isDigit then card2.apply(i).asDigit else mapDict(card2.apply(i))
        if _card1 > _card2 then return false
        else if _card1 < _card2 then return true
      }
      false
    }

    def joker(card: String): Option[Char] = {
      val remove_joker = card.replace("J", "")
      remove_joker.length match
        case x if x < 5 =>
          val best_type = remove_joker.toArray.groupMapReduce(i => i)(_ => 1)(_ + _).toArray
          if best_type.length > 0 then Some(best_type.maxBy(_._2)._1) else Some('J')
        case _ => None
    }

    def map_type(hand: String, bet: Int): Map[String, Array[(String, Int)]] = {
      val new_cards = joker(hand) match
        case Some(card) =>
          hand.replace('J', card).toArray
        case _ => hand.toArray
      val frequency = new_cards.groupMapReduce(i => i)(_ => 1)(_ + _).toArray
      val typeOfCard = frequency.length match {
        case 1 => "five_kind"
        case 2 => if frequency.exists((_, x) => x > 3) then "four_kind" else "full_house"
        case 3 => if frequency.exists((_, x) => x > 2) then "three_kind" else "two_pair"
        case 4 => "one_pair"
        case _ => "high_card"
      }

      val updatedHighCard = pokerHands(typeOfCard) :+ (hand, bet)
      val sortedUpdate = updatedHighCard.sortWith((elem1, elem2) => compare_card(elem1._1, elem2._1))
      pokerHands = pokerHands.updated(typeOfCard, sortedUpdate)
      pokerHands
    }

    for (line <- lines) {
      val Array(hand, bet) = line.split(" ")
      map_type(hand, bet.toInt)
    }

    var result: Array[Int] = Array.empty

    // Add to result by order
    handOrder.foreach { handType =>
      pokerHands.get(handType).foreach { cards =>
        cards.foreach(card => {
          result = result :+ card._2
        })
      }
    }

    println(result.zipWithIndex.map((x, y) => x * (y + 1)).sum)
  }
}
