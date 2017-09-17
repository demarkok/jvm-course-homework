package evaluation

import scala.collection.mutable

trait RpnEvaluationEntity[R] {
  def evaluate(rpmStack: mutable.ListBuffer[R]): Unit
}
