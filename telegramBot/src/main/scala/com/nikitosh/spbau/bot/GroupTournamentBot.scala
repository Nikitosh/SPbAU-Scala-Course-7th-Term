package com.nikitosh.spbau.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import com.nikitosh.spbau.database.GroupTournamentActor._
import com.nikitosh.spbau.database.Tournament
import com.nikitosh.spbau.parser.MessageParser
import com.nikitosh.spbau.parser.messages.{
  AddResult => AddResultMessage,
  CreateTournament => CreateTournamentMessage,
  _
}
import info.mukel.telegrambot4s.models.Message

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Success

class GroupTournamentBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {
  def askDatabase(name: String, function: (Option[Tournament]) => Unit)(implicit timeout: Timeout,
                                                                        message: Message): Unit = {
    (database ? GetTournament(name)).onComplete {
      case Success(TournamentMessage(maybeTournament)) =>
        function(maybeTournament)
      case _ =>
        reply("Database error!")
    }
  }

  onMessage { implicit message =>
    message.text.foreach { text =>
      implicit val timeout: Timeout = Timeout(1.second)
      MessageParser.parse(text) match {
        case CreateTournamentMessage(name, players) =>
          askDatabase(
            name, {
              case None =>
                database ! CreateTournament(name, players)
                reply("Tournament was created successfully!")
              case _ =>
                reply("Sorry, tournament with such name already exists. Please try something different.")
            }
          )

        case AddResultMessage(name, player1, player2, player1Score, player2Score) =>
          askDatabase(
            name, {
              case Some(tournament) =>
                if (!tournament.hasPlayer(player1)) {
                  reply("Sorry, there is no user with such name: " + player1 + " in this tournament.")
                } else if (!tournament.hasPlayer(player2)) {
                  reply("Sorry, there is no user with such name: " + player2 + " in this tournament.")
                } else if (player1 == player2) {
                  reply("Sorry, players should not be same person.")
                } else if (tournament.games.exists(game =>
                             (game.player1 == player1 && game.player2 == player2)
                               || (game.player1 == player2 && game.player2 == player1))) {
                  reply("Sorry, these players already played a game before.")
                } else {
                  database ! AddResult(name, player1, player2, player1Score, player2Score)
                  reply("Result was added successfully!")
                }
              case _ =>
                reply("Sorry, there is no tournament with such name")
            }
          )

        case Games(name) =>
          askDatabase(
            name, {
              case Some(tournament) =>
                reply(
                  "Games list:\n" +
                    tournament.games
                      .map(game =>
                        game.player1 + " " + game.player1Score + ":" + game.player2Score + " " + game.player2)
                      .mkString("\n"))
              case _ =>
                reply("Sorry, there is no tournament with such name")
            }
          )
        case MyGames(name) =>
          askDatabase(
            name, {
              case Some(tournament) =>
                val username = message.from.get.username.get
                reply(
                  "Games list:\n" +
                    tournament.games
                      .filter(game => game.player1 == username || game.player2 == username)
                      .map(game =>
                        game.player1 + " " + game.player1Score + ":" + game.player2Score + " " + game.player2)
                      .mkString("\n"))
              case _ =>
                reply("Sorry, there is no tournament with such name")
            }
          )
        case Results(name) =>
          askDatabase(
            name, {
              case Some(tournament) =>
                val points = mutable.Map[String, Int]()
                tournament.players.foreach(player => points.update(player, 0))
                tournament.games
                  .foreach(game => points(game.getWinner) += 1)
                reply(
                  (points.toSeq.sortWith(_._2 > _._2) map {
                    case (player, playerPoints) => player + ": " + playerPoints
                  }).mkString("\n"))
              case _ =>
                reply("Sorry, there is no tournament with such name")
            }
          )
        case WrongMessage =>
          reply("Wrong command!")
      }
    }
  }
}
