package rpn

import evaluation.ArithmeticEvaluationEntity
import parsing.ArithmeticToken

import scala.collection.mutable.ListBuffer

object ShuntingYard {
  def toRPN(tokens: List[ArithmeticToken]): List[ArithmeticEvaluationEntity] = {
    val entities = tokens.map(x => ArithmeticEntity(x))
    val rpnResult: ListBuffer[ArithmeticEvaluationEntity] = ListBuffer.empty
    val operatorStack: ListBuffer[ArithmeticEntity] = ListBuffer.empty
    def shift() = rpnResult += ArithmeticEvaluationEntity(operatorStack.remove(0))
    entities.foreach {
      case number: Number => rpnResult += ArithmeticEvaluationEntity(number)
      case operator: Operator =>
        def condition(entity: ArithmeticEntity): Boolean = {
          entity match {
            case operator2: Operator if operator.isLeftAssoc &&
              operator.priority < operator2.priority => true
            case operator2: Operator if operator.isLeftAssoc &&
              operator.priority <= operator2.priority => true
            case _ => false
          }
        }

        while (operatorStack.headOption.exists(condition)) {
          shift()
        }
        operator +=: operatorStack
      case br @ OpeningBracket => br +=: operatorStack
      case ClosingBracket =>
        while (operatorStack.headOption.exists(_ != OpeningBracket)) {
          shift()
        }
        if (operatorStack.isEmpty) {
          throw new RuntimeException
        }
        operatorStack.remove(0)
    }
    while (operatorStack.nonEmpty) {
      operatorStack.remove(0) match {
        case op: Operator => rpnResult += ArithmeticEvaluationEntity(op)
        case _ => throw new RuntimeException
      }
    }
    rpnResult.toList
  }
}
