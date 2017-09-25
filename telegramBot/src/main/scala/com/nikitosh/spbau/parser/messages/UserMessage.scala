package com.nikitosh.spbau.parser.messages

trait UserMessage

case class CreateTournament(name: String, players: List[String]) extends UserMessage

case class AddResult(name: String, player1: String, player2: String, player1Score: Int, player2Score: Int)
    extends UserMessage

case class Games(name: String) extends UserMessage

case class MyGames(name: String) extends UserMessage

case class Results(name: String) extends UserMessage

case object WrongMessage extends UserMessage
