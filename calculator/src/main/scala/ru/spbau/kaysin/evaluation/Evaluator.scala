package ru.spbau.kaysin.evaluation

import scala.collection.mutable.ListBuffer

/**
  * Represents the evaluation which uses revers Polish notation.
  * @tparam R The evaluation result.
  */
class Evaluator[R] {
  /**
    * Performs the evaluation.
    * @param list list of evaluation entities wrote in rpn.
    * @return the result of evaluation
    */
  def evaluate(list: List[EvaluationEntity[R]]): R = {
    val rpnBuffer: ListBuffer[R] = ListBuffer.empty
    list.foreach(_.evaluate(rpnBuffer))
    rpnBuffer.head
  }
}
