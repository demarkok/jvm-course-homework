import evaluation.Evaluator
import parsing.Tokenizer
import rpn.ShuntingYard

object Main {

  def main(args: Array[String]): Unit = {

    val input = scala.io.StdIn.readLine() : String

    val tokens = Tokenizer.tokenize(input)
    val rpn = ShuntingYard.toRPN(tokens.get)
    val evaluator = new Evaluator[Double]

    println(evaluator.evaluate(rpn))
  }
}
