package com.nikitosh.spbau.database

import akka.persistence.PersistentActor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GroupTournamentActor extends PersistentActor {

  import GroupTournamentActor._

  val tournaments: mutable.HashMap[String, Tournament] = mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case CreateTournament(tournamentName, players) =>
        tournaments(tournamentName) = new Tournament(players)
      case AddResult(tournamentName, player1, player2, player1Score, player2Score) =>
        tournaments(tournamentName).addGame(new Game(player1, player2, player1Score, player2Score))
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetTournament(tournamentName: String) =>
      sender ! TournamentMessage(tournaments.get(tournamentName))
  }

  override def persistenceId: String = "group-tournament-database"
}

object GroupTournamentActor {

  //events
  trait Event

  case class CreateTournament(tournamentName: String, players: List[String]) extends Event

  case class AddResult(tournamentName: String, player1: String, player2: String, player1Score: Int, player2Score: Int)
      extends Event

  //queries
  case class GetTournament(tournamentName: String)

  case class TournamentMessage(tournament: Option[Tournament])
}

class Tournament(private val _players: List[String]) {
  private val _games: mutable.Buffer[Game] = ArrayBuffer[Game]()
  def games: mutable.Buffer[Game]          = _games
  def players: List[String]                = _players

  def hasPlayer(player: String): Boolean = _players.contains(player)

  def addGame(game: Game): Unit = _games += game
}

class Game(val player1: String, val player2: String, val player1Score: Int, val player2Score: Int) {
  def getWinner: String = {
    if (player1Score > player2Score) {
      player1
    } else {
      player2
    }
  }
}
