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



class HatBotActor(val token: String,
                  val database: ActorRef) extends TelegramBot with Polling with Commands with Actor {

  implicit val timeout: Timeout = Timeout(60 seconds)

  import HatBotActor._

  private val states: mutable.HashMap[Long, StateHolder] = mutable.HashMap.empty
  private val games: mutable.HashMap[Long, ActorRef] = mutable.HashMap.empty

  onMessage { implicit message =>
    implicit val id: Long = message.chat.id
    message.text.foreach { text =>
      val userState = states.getOrElseUpdate(id, {
        send("Привет. Это эмулятор шляпы. Со шляпой можно делать две вещи: " +
          "добавлять в неё слова (даже с разных устройств) и играть (с одного устройства).")(id)
        val state = StateHolder(this)
        state
      })

      val command: UserMessage = MessageParser.parser(text)
      userState.state match  {
        case MainMenu => command match {
          case AddWords =>
            userState.changeState(HatWaitingAdd)
          case Play =>
            userState.changeState(HatWaitingPlay)
          case Start =>
            userState.changeState(MainMenu)
          case _ =>
            send("Неправильная команда.")
        }

        case HatWaitingAdd => command match {
          case Start =>
            userState.changeState(MainMenu)
          case UserWord(hat) =>
            userState.changeState(Adding(hat))
          case _ =>
            send("Имя шляпы должно состоять из одного слова.")
        }

        case Adding(hat) => command match {
          case Start =>
            userState.changeState(MainMenu)
          case EndOfInput =>
            userState.changeState(MainMenu)
            send(hidingMessage)
          case UserWord(word) =>
            database ! AddWord(hat, word)
          case _ =>
            send("Неправильное слово.")
        }

        case HatWaitingPlay => command match {
          case Start =>
            userState.changeState(MainMenu)
          case UserWord(hat) =>
            val game = context.actorOf(GameActor.props(hat, database))
            games.update(id, game)
            userState.changeState(Interlude(game))
          case _ =>
            send("Имя шляпы должно состоять из одного слова.")
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
                send("Эта шляпа пуста. Добавьте в неё слова или выберете другю.")
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
                send(s"Слов угадано: $wordsGuessed")
                send("Игра закончена, шляпа пуста!")
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
            send(s"Время вышло. Слов угадано: $wordsGuessed")(id)
            stateHolder.changeState(Interlude(game))
          case _ =>
        }
      }
    case TimeIsRunningOut(id, left) =>
      states.get(id).foreach { stateHolder =>
        stateHolder.state match {
          case Round(_) =>
            send(s"Осталось ${left}с")(id)
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

  def props(token: String, database: ActorRef): Props = Props(classOf[HatBotActor], token, database)

  case class StateHolder(bot: HatBotActor, var state: BotState = MainMenu)(implicit id: Long) {
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

  sealed trait WordResponse

  case object Run
  case class HatIsEmpty(wordsGuessed: Int) extends WordResponse
  case class Word(word: String) extends WordResponse

  case class TimeIsUp(id: Long, wordsGuessed: Int)
  case class TimeIsRunningOut(id: Long, secsLeft: Int)

  private val hidingMessage = "\n*** Скрывающее сообщение ***\n\n" * 5
  val roundTime: Int = 20
}
