package evaluation

import rpn.ArithmeticEntity

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait ArithmeticEvaluationEntity extends RpnEvaluationEntity[Double]

object ArithmeticEvaluationEntity {
  def apply(entity: ArithmeticEntity): ArithmeticEvaluationEntity = entity match {
    case rpn.Plus => Plus
    case rpn.Minus => Minus
    case rpn.Mul => Multiplication
    case rpn.Div => Division
    case rpn.Number(value) => new Value(value)
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
    val first = rpmStack.remove(0)
    val second = rpmStack.remove(0)
    function2(first, second) +=: rpmStack
  }
}

object Plus extends Operator {
  override val function2: (Double, Double) => Double = _+_
}

object Minus extends Operator {
  override val function2: (Double, Double) => Double = _-_
}

object Multiplication extends Operator {
  override val function2: (Double, Double) => Double = _*_
}

object Division extends Operator {
  override val function2: (Double, Double) => Double = _/_
}

