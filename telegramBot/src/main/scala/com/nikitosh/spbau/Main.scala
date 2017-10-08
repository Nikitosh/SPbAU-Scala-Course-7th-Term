package com.nikitosh.spbau

import akka.actor.{ActorSystem, Props}
import com.nikitosh.spbau.bot.GroupTournamentBot
import com.nikitosh.spbau.database.GroupTournamentActor

object Main extends App {
  val token = "347362937:AAEknhWHWN7pIPlxY0qzvOd-NKM7i0nwoCk"

  val system   = ActorSystem()
  val database = system.actorOf(Props(classOf[GroupTournamentActor]))

  private val bot = new GroupTournamentBot(token, database)

  bot.run()
}
