package evaluation

import parsing.{ParsingException, Token}

import scala.collection.mutable.ListBuffer

/**
  * The object implementing the <a href="https://en.wikipedia.org/wiki/Shunting-yard_algorithm">shunting-yard algorithm</a>
  * which transforms a list of tokens into <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">reverse Polish notation</a>
  */
object ShuntingYard {
  /**
    * transforms the list of tokens into list of arithmetic evaluation entities wrote in reverse Polish notation.
    * It can be simply generalized to return rpn of any EvaluationEntity, not only of arithmetic one
    * (e.g. we can evaluate expression into ast)
    * @param tokens list of tokens, input of the algorithm
    * @return list of arithmetic evaluation entities in reverse Polish notation
    */
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
          throw ParsingException
        }
        operatorStack.remove(0)
    }
    while (operatorStack.nonEmpty) {
      operatorStack.remove(0) match {
        case token@Token(_:parsing.Operator, _) => rpnResult += ArithmeticEvaluationEntity(token)
        case _ => throw ParsingException
      }
    }
    rpnResult.toList
  }
}
