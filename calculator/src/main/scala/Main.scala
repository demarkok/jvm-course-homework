import evaluation.{Evaluator, ShuntingYard}
import parsing.Tokenizer

object Main {

  /**
    * The emulation of calculator. It takes an expression from the input stream and print the result to the output stream.
    * @param args input arguments. Supposed to be empty.
    */
  def main(args: Array[String]): Unit = {

    val input = scala.io.StdIn.readLine() : String

    val tokens = Tokenizer.tokenize(input)
    val rpn = ShuntingYard.toRPN(tokens.get)
    val evaluator = new Evaluator[Double]

    println(evaluator.evaluate(rpn))
  }
}
