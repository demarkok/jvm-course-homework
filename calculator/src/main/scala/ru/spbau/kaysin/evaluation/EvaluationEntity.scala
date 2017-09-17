package ru.spbau.kaysin.evaluation

import scala.collection.mutable

/**
  * The trait describing entity which can perform ru.spbau.kaysin.evaluation via evaluating stack
  * @tparam R result of the ru.spbau.kaysin.evaluation
  */
trait EvaluationEntity[R] {
  /**
    * perform the ru.spbau.kaysin.evaluation. Entity should take some parameters from the head of the stack and put the result into it.
    * @param evalStack evaluationStack the entity interact with
    */
  def evaluate(evalStack: mutable.ListBuffer[R]): Unit
}
