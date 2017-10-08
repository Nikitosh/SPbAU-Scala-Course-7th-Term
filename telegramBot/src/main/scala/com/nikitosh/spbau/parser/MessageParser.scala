package com.nikitosh.spbau.parser

import com.nikitosh.spbau.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override def skipWhitespace: Boolean = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  val wordParser: Parser[String] = raw"[a-zA-Z0-9]+".r
  val intParser: Parser[Int] = "(0|[1-9][0-9]*)".r ^^ {
    _.toInt
  }
  val listParser: Parser[List[String]] = rep1sep(wordParser, ",")

  val createTournament: Parser[UserMessage] = "[Cc]reate tournament".r ~> wordParser ~ listParser ^^ {
    case name ~ players => CreateTournament(name, players)
  }

  val addResult
    : Parser[UserMessage] = "[Aa]dd result".r ~> wordParser ~ wordParser ~ intParser ~ (":" ~> intParser) ~ wordParser ^^ {
    case name ~ player1 ~ player1Score ~ player2Score ~ player2 =>
      AddResult(name, player1, player2, player1Score, player2Score)
  }

  val games: Parser[UserMessage] = "[Gg]ames".r ~> wordParser ^^ { name =>
    Games(name)
  }

  val myGames: Parser[UserMessage] = "[Mm]y games".r ~> wordParser ^^ { name =>
    MyGames(name)
  }

  val results: Parser[UserMessage] = "[Rr]esults".r ~> wordParser ^^ { name =>
    Results(name)
  }

  val userMessage: Parser[UserMessage] =
    createTournament | addResult | games | myGames | results
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _                   => WrongMessage
    }
  }
}
