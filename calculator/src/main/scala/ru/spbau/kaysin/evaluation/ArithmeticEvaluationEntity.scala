package ru.spbau.kaysin.evaluation

import ru.spbau.kaysin.parsing._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * The trait describing an entity related to arithmetic.
  * E.g. it might be number, operator or function.
  * It should return Double as ru.spbau.kaysin.evaluation result.
  */
sealed trait ArithmeticEvaluationEntity extends EvaluationEntity[Double]

/**
  * The companion object of ArithmeticEvaluationEntity trait
  */
object ArithmeticEvaluationEntity {

  /**
    * Constructs ArithmeticEvaluationEntity from arithmetic token.
    * @param token arithmetic token
    * @return corresponding ru.spbau.kaysin.evaluation entity
    */
  def apply(token: Token): ArithmeticEvaluationEntity = token match {
    case Token(PlusType, _) => Plus
    case Token(MinusType, _) => Minus
    case Token(MulType, _) => Mul
    case Token(DivType, _) => Div
    case Token(NumberType, value) => new Value(value.toDouble)
    case _ => throw new RuntimeException
  }
}

/**
  * The class of eval. entities which evaluate into themselves
  * @param value holding real number
  */
class Value(value: Double) extends ArithmeticEvaluationEntity {
  override def evaluate(rpmStack: ListBuffer[Double]): Unit = {
    value +=: rpmStack
  }
}

/**
  * The trait describing eval. entities corresponding to arithmetic operations.
  */
sealed trait Operator extends ArithmeticEvaluationEntity {
  /**
    * The function corresponding to the arithmetic operation.
    */
  val function2: (Double, Double) => Double

  /**
    * @inheritdoc
    */
  override def evaluate(rpmStack: mutable.ListBuffer[Double]): Unit = {
    if (rpmStack.length < 2) {
      throw EvaluationException
    }
    val second = rpmStack.remove(0)
    val first = rpmStack.remove(0)
    function2(first, second) +=: rpmStack
  }
}

/**
  * Eval. entity corresponding to arithmetic '+'.
  */
object Plus extends Operator {
  override val function2: (Double, Double) => Double = _+_
}

/**
  * Eval. entity corresponding to arithmetic '-'.
  */
object Minus extends Operator {
  override val function2: (Double, Double) => Double = _-_
}

/**
  * Eval. entity corresponding to arithmetic '*'.
  */
object Mul extends Operator {
  override val function2: (Double, Double) => Double = _*_
}

/**
  * Eval. entity corresponding to arithmetic '/'.
  */
object Div extends Operator {
  override val function2: (Double, Double) => Double = _/_
}

