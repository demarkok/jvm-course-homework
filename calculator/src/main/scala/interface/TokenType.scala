package interface

import scala.util.matching.Regex

trait TokenType {
  val regex: Regex
}
