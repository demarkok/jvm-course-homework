package parsing

trait Tokenizer {
  def tokenize(string: String): Option[List[Token]]
}
