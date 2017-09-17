package evaluation

import scala.collection.mutable

/**
  * The trait describing entity which can perform evaluation via evaluating stack
  * @tparam R result of the evaluation
  */
trait EvaluationEntity[R] {
  /**
    * perform the evaluation. Entity should take some parameters from the head of the stack and put the result into it.
    * @param evalStack evaluationStack the entity interact with
    */
  def evaluate(evalStack: mutable.ListBuffer[R]): Unit
}
