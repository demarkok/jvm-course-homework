package evaluation

import parsing.Token

import scala.collection.mutable.ListBuffer

object ShuntingYard {
  def toRPN(tokens: List[Token]): List[ArithmeticEvaluationEntity] = {

    val rpnResult: ListBuffer[ArithmeticEvaluationEntity] = ListBuffer.empty
    val operatorStack: ListBuffer[Token] = ListBuffer.empty
    def shift() = rpnResult += ArithmeticEvaluationEntity(operatorStack.remove(0))
    tokens.foreach {
      case number@Token(parsing.Number, _) => rpnResult += ArithmeticEvaluationEntity(number)
      case opToken@Token(operator: parsing.Operator, _) =>
        def condition(token: Token): Boolean = {
          token.tokenType match {
            case operator2: parsing.Operator if operator.isLeftAssoc &&
              operator.priority < operator2.priority => true
            case operator2: parsing.Operator if operator.isLeftAssoc &&
              operator.priority <= operator2.priority => true
            case _ => false
          }
        }

        while (operatorStack.headOption.exists(condition)) {
          shift()
        }
        opToken +=: operatorStack
      case br@Token(parsing.OpeningBracket, _) => br +=: operatorStack
      case Token(parsing.ClosingBracket, _) =>
        while (operatorStack.headOption.exists(_.tokenType != parsing.OpeningBracket)) {
          shift()
        }
        if (operatorStack.isEmpty) {
          throw new RuntimeException
        }
        operatorStack.remove(0)
    }
    while (operatorStack.nonEmpty) {
      operatorStack.remove(0) match {
        case token@Token(_:parsing.Operator, _) => rpnResult += ArithmeticEvaluationEntity(token)
        case _ => throw new RuntimeException
      }
    }
    rpnResult.toList
  }
}
