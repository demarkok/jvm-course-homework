import evaluation.Evaluator
import parsing.{ArithmeticTokenType, ArithmeticTokenizer, Tokenizer}
import rpn.ShuntingYard

import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]): Unit = {

    val input = scala.io.StdIn.readLine() : String

    val tokens = ArithmeticTokenizer.tokenize(input)
    val rpn = ShuntingYard.toRPN(tokens.get)
    val evaluator = new Evaluator[Double]

    println(evaluator.evaluate(rpn))
  }
}
