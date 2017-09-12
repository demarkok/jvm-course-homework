package interface

trait Tokenizer {
  def tokenize(string: String): Option[List[Token]]
}
