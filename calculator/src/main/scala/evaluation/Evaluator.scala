package evaluation

import scala.collection.mutable.ListBuffer

class Evaluator[R] {
  def evaluate(list: List[RpnEvaluationEntity[R]]): R = {
    val rpnBuffer: ListBuffer[R] = ListBuffer.empty
    list.foreach(_.evaluate(rpnBuffer))
    rpnBuffer.head
  }
}
