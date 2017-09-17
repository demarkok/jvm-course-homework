package parsing

import scala.util.matching.Regex

trait TokenType {
  val regex: Regex
}
