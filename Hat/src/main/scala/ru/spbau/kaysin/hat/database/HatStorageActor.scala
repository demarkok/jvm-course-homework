package ru.spbau.kaysin.hat.database

import akka.actor.Props
import akka.persistence.PersistentActor
import ru.spbau.kaysin.hat.game.GameActor.Words

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * An actor representing database which contains all the hats. Each hat contains a list of words which were added to it.
  */
class HatStorageActor extends PersistentActor {

  import HatStorageActor._

  /**
    * A map from hat name to list of the words in this hat.
    */
  val hats: mutable.HashMap[String, ListBuffer[String]] = mutable.HashMap.empty // hatName -> words

  /**
    * Receive an event (a message modifying database).
    * @param event The event message.
    */
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

  /**
    * Function allowing to create the actor in a safe way.
    * @return Props object which is passed to system.actorOf
    */
  def props(): Props = Props(new HatStorageActor())

  /**
    * A trait representing a Message modifying database
    */
  trait Event

  /**
    * Add word into database.
    * @param hat - the name of the hat in which the word is added
    * @param word - added word
    */
  case class AddWord(hat: String, word: String) extends Event

  /**
    * Get all the words contained in the hat.
    * @param hat - the name of the hat
    */
  case class GetWords(hat: String)
}
