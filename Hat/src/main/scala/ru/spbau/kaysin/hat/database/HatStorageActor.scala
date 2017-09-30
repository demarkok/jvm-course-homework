package ru.spbau.kaysin.hat.database

import akka.actor.Props
import akka.persistence.PersistentActor
import ru.spbau.kaysin.hat.game.GameActor.Words

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class HatStorageActor extends PersistentActor {

  import HatStorageActor._

  val hats: mutable.HashMap[String, ListBuffer[String]] = mutable.HashMap.empty // hatName -> words

  def receiveEvent(event: HatStorageActor.Event): Unit = event match {
      case AddWord(hat, word) =>
        hats.getOrElseUpdate(hat, ListBuffer.empty) += word
  }


  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetWords(hat) =>
      sender ! Words(hats.getOrElse(hat, List.empty).toList)
  }

  override def persistenceId = "hat-storage-actor"
}

object HatStorageActor {

  def props(): Props = Props(new HatStorageActor())

  trait Event
  case class AddWord(hat: String, word: String) extends Event
  case class GetWords(hat: String)
}
