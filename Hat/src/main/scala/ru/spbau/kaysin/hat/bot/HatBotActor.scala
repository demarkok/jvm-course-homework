package ru.spbau.kaysin.hat.bot

import akka.actor.{Actor, ActorRef, Kill, Props}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.Implicits._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.{KeyboardButton, Message, ReplyKeyboardMarkup}
import ru.spbau.kaysin.hat.database.HatStorageActor.AddWord
import ru.spbau.kaysin.hat.game.GameActor
import ru.spbau.kaysin.hat.game.GameActor.Guessed
import ru.spbau.kaysin.hat.parser._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._


/**
  * The main actor corresponding to the bot (one for all the connections)
  * @param token telegram bot token
  * @param database database actor keeping all the words (HatStorageActor)
  */
class HatBotActor(val token: String,
                  val database: ActorRef) extends TelegramBot with Polling with Commands with Actor {

  private implicit val timeout: Timeout = Timeout(60 seconds)

  import HatBotActor._

  private val states: mutable.HashMap[Long, StateHolder] = mutable.HashMap.empty
  private val games: mutable.HashMap[Long, ActorRef] = mutable.HashMap.empty

  onMessage { implicit message =>
    implicit val id: Long = message.chat.id
    message.text.foreach { text =>
      val userState = states.getOrElseUpdate(id, {
        send(welcome)
        StateHolder(this)
      })

      val command: UserMessage = MessageParser.parse(text)
      userState.state match  {
        case MainMenu => command match {
          case AddWords =>
            userState.changeState(HatWaitingAdd)
          case Play =>
            userState.changeState(HatWaitingPlay)
          case Start =>
            userState.changeState(MainMenu)
          case Rules =>
            send(rules)
            userState.changeState(MainMenu)
          case _ =>
            send("Unknown command =(.")
        }

        case HatWaitingAdd => command match {
          case Start =>
            userState.changeState(MainMenu)
          case UserWord(hat) =>
            userState.changeState(Adding(hat))
          case _ =>
            send("Hat name is one word")
        }

        case Adding(hat) => command match {
          case Start =>
            userState.changeState(MainMenu)
          case EndOfInput =>
            send(hidingMessage)
            userState.changeState(MainMenu)
          case UserWord(word) =>
            database ! AddWord(hat, word)
          case _ =>
            send("Incorrect word")
        }

        case HatWaitingPlay => command match {
          case Start =>
            userState.changeState(MainMenu)
          case UserWord(hat) =>
            val game = context.actorOf(GameActor.props(hat, database))
            games.update(id, game)
            userState.changeState(Interlude(game))
          case _ =>
            send("Hat name is one word")
        }

        case Interlude(game) => command match {
          case Start => // start the bot, not the round
            userState.changeState(MainMenu)
            gameOver(id)
          case StartRound =>
            userState.changeState(Round(game))
            game ! GameActor.Start(roundTime)
            askForAWord(game) match {
              case HatIsEmpty(_) =>
                send("This hat is empty. Add words to it or choose another.")
                userState.changeState(MainMenu)
              case Word(word) =>
                send(word)
            }
          case _ =>
        }

        case Round(game) => command match {
          case Start =>
            userState.changeState(MainMenu)
            gameOver(id)
          case NextWord =>
            game ! Guessed
            askForAWord(game) match {
              case HatIsEmpty(wordsGuessed) =>
                send(s"Words guessed: $wordsGuessed")
                send("Game over! The hat is empty.")
                userState.changeState(MainMenu)
                gameOver(id)
              case Word(word) =>
                send(word)
            }
          case _ =>
        }
      }
    }
  }

  override def receive: Receive = {
    case TimeIsUp(id, wordsGuessed) =>
      states.get(id).foreach { stateHolder =>
        stateHolder.state match {
          case Round(game) =>
            send(hidingMessage)(id)
            send(s"Time is up! Words guessed: $wordsGuessed")(id)
            stateHolder.changeState(Interlude(game))
          case _ =>
        }
      }
    case TimeIsRunningOut(id, left) =>
      states.get(id).foreach { stateHolder =>
        stateHolder.state match {
          case Round(_) =>
            send(s"${left}s left!")(id)
          case _ =>
        }
      }
    case Run =>
      run
  }

  private def gameOver(id: Long): Unit = {
    games.get(id).foreach { game =>
      game ! Kill
    }
    games.remove(id)
  }

  private def askForAWord(game: ActorRef)(implicit id: Long, message: Message): WordResponse = {
    Await.result(game ? GameActor.GetWord, timeout.duration).asInstanceOf[WordResponse]
  }

  private def send(message: String,
           keyboard: Option[List[String]] = None,
           oneTimeKeyboard: Boolean = true)(implicit id: Long): Unit = {
    val markup = keyboard.map(ss =>
      ReplyKeyboardMarkup.singleColumn(ss.map(s => KeyboardButton(s)), oneTimeKeyboard = oneTimeKeyboard))
    Await.result(request(SendMessage(id, message, replyMarkup = markup)), timeout.duration)
  }

}


object HatBotActor {

  /**
    * Function allowing to create the actor in a safe way.
    * @param token - telegram token argument which is passed to HatBotActor's constructor
    * @param database - database actor which is passed to HatBotActor's constructor
    * @return Props object which is passed to system.actorOf
    */
  def props(token: String, database: ActorRef): Props = Props(classOf[HatBotActor], token, database)

  /**
    * State holder which wraps a bot state and allows to add side-effects to state switching
    * @param bot HatBotActor who's methods (send) the state holder asks.
    * @param state A held state.
    * @param id Chat id which is used to send messages
    */
  case class StateHolder(bot: HatBotActor, var state: BotState = MainMenu)(implicit id: Long) {

    /**
      * Change state. All the side effects are here.
      * @param newState new held state.
      */
    def changeState(newState: BotState): Unit = {
      newState match {
        case withKeyboard: WithCommands =>
          bot.send(withKeyboard.message, withKeyboard.buttons, withKeyboard.oneTime)
        case _ =>
          bot.send(newState.message)
      }
      state = newState
    }
  }

  /**
    * A response which occurs when you ask a new word.
    */
  sealed trait WordResponse

  /**
    * A response which occurs when there's no more words in the hat
    * @param wordsGuessed Number of words guessed in this round.
    */
  case class HatIsEmpty(wordsGuessed: Int) extends WordResponse

  /**
    * A response wrapping a new word from the hat.
    * @param word A got word
    */
  case class Word(word: String) extends WordResponse


  /**
    * A query to run the bot.
    */
  case object Run

  /**
    * A message which occurs when round time is up.
    * @param id Id of the chat in which the game is running.
    * @param wordsGuessed Number of words guessed in this round.
    */
  case class TimeIsUp(id: Long, wordsGuessed: Int)

  /**
    *
    * @param id Id of the chat in which the game is running.
    * @param secsLeft Time (in seconds) until the end of round.
    */
  case class TimeIsRunningOut(id: Long, secsLeft: Int)

  private val hidingMessage: String = ".\n" * 40

  /**
    * Time of game round in seconds.
    */
  val roundTime: Int = 20

  private val welcome: String = "Hi! This is the hat game emulator. You can do two things with the hat: add new words to it or play it." +
    "You can use several devices to ADD WORDS into one hat if you select the same hat name. But you should use one device " +
    s"to PLAY the same game. For more specific rules write /${Rules.command}."

  private val rules: String = "Firstly, let's describe real analogue of the game.\n Firs of all, every player writes several " +
    "nouns on small pieces of paper and throws it into the hat. Then the players divide into pairs. One pair = one team. " +
    "The players sit in a circle. Players from one pair should sit across from each other. " +
    "The hat is passed through the circle.\nThe game consists of several round lasting 20 seconds. " +
    "In one round one pair plays. One person of the pair should " +
    "explain words and another should guess them. The explainer gets random words from the hat one by one (he doesn't pull out " +
    "a new word until the old word is guessed by the guesser). The explainer shouldn't mention words with the same root as well as " +
    "ones which sounds alike as well as the letters of the word.\n" +
    "If the word wasn't guessed, it returns into the hat and can be explained in further rounds. " +
    "If the word was guessed, it brings one point to the team. The game ends when the hat is empty. The team with max amount of" +
    " points wins the game.\n" +
    "This bot provides you infinite number of hats. You can add words into a hat and these words will permanently be in this " +
    "hat (they will be restored after a round), so you can use several devices to add words into the same hat. " +
    "Also you can play the game using some hat. But you should use one device for one game."
}
