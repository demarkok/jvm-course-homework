import ru.spbau.kaysin.evaluation.{EvaluationException, Evaluator, ShuntingYard}
import ru.spbau.kaysin.parsing.{Tokenizer, ParsingException}

object Main {

  private val evaluationErrorMsg: String = "Evaluation error."
  private val parsingErrorMsg: String = "Parsing error."
  private val errorMsg: String = "Error."

  /**
    * The emulation of calculator. It takes an expression from the input stream and print the result to the output stream.
    *
    * @param args input arguments. Supposed to be empty.
    */
  def main(args: Array[String]): Unit = {

    val input = scala.io.StdIn.readLine() : String

    val tokens = Tokenizer.tokenize(input)
    if (tokens.isEmpty) {
      println(parsingErrorMsg)
      return
    }

    try {
      val rpn = ShuntingYard.toRPN(tokens.get)
      val evaluator = new Evaluator[Double]

      println(evaluator.evaluate(rpn))
    } catch {
      case ParsingException => println(parsingErrorMsg)
      case EvaluationException => println(evaluationErrorMsg)
      case _: Exception => println(errorMsg)
    }


  }
}
