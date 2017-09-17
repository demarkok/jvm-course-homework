package ru.spbau.kaysin.evaluation

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSuite
import ru.spbau.kaysin.parsing._

class EvaluationTest extends FunSuite {
  val eps: Double = 1e-6
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(eps)

  test("test single number") {
    assert(eval("1") === 1.0)
  }

  test("test plus") {
    assert(eval("100 + 139") === 239.0)
  }

  test("test minus") {
    assert(eval("439 - 200") === 239.0)
  }

  test("test multiplication") {
    assert(eval("3.14 * 10") === 31.4)
  }

  test("test division") {
    assert(eval("3 / 4") === 0.75)
  }

  test("test priority") {
    assert(eval("2 + 2 * 2") === 6.0)
  }

  test("test parentheses") {
    assert(eval("(2 + 2) * 2") === 8.0)
  }

  test("test pointed") {
    assert(eval("239 / 0.001") === 239000.0)
  }

  test("8 * 10 / 2 - 1 + 2 * 10 * ((1 + 2) * 3 + 1)") {
    assert(eval("8 * 10 / 2 - 1 + 2 * 10 * ((1+2) * 3 + 1)") === 239.0)
  }

  def eval(string: String): Double = {
    val tokens = Tokenizer.tokenize(string)
    val rpn = ShuntingYard.toRPN(tokens.get)
    val evaluator = new Evaluator[Double]
    evaluator.evaluate(rpn)
  }

}
