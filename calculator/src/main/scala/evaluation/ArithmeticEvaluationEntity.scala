package evaluation

import parsing.Token

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait ArithmeticEvaluationEntity extends RpnEvaluationEntity[Double]

object ArithmeticEvaluationEntity {
  def apply(token: Token): ArithmeticEvaluationEntity = token match {
    case Token(parsing.Plus, _) => Plus
    case Token(parsing.Minus, _) => Minus
    case Token(parsing.Mul, _) => Mul
    case Token(parsing.Div, _) => Div
    case Token(parsing.Number, value) => new Value(value.toDouble)
    case _ => throw new RuntimeException
  }
}

class Value(value: Double) extends ArithmeticEvaluationEntity {
  override def evaluate(rpmStack: ListBuffer[Double]): Unit = {
    value +=: rpmStack
  }
}

sealed abstract class Operator extends ArithmeticEvaluationEntity {
  val function2: (Double, Double) => Double

  override def evaluate(rpmStack: mutable.ListBuffer[Double]): Unit = {
    if (rpmStack.length < 2) {
      throw new RuntimeException
    }
    val second = rpmStack.remove(0)
    val first = rpmStack.remove(0)
    function2(first, second) +=: rpmStack
  }
}

object Plus extends Operator {
  override val function2: (Double, Double) => Double = _+_
}

object Minus extends Operator {
  override val function2: (Double, Double) => Double = _-_
}

object Mul extends Operator {
  override val function2: (Double, Double) => Double = _*_
}

object Div extends Operator {
  override val function2: (Double, Double) => Double = _/_
}

