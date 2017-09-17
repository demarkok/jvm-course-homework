package parsing

import scala.util.matching.Regex

/**
  * Represents arithmetic tokens. E.g. number, bracket, operator, dot symbol, comma, etc.
  */
sealed trait TokenType {
  /**
    * The regex which matches this type of tokens.
    */
  val regex: Regex
  TokenType.instances :+= this
}

/**
  * the companion object for TokenType
  */
object TokenType {
  private var instances: List[TokenType] = List.empty

  /**
    * The getter for list of all token types.
    * @return the list of all token types.
    */
  def getInstances: List[TokenType] = instances

  // I should initialize the instances beforehand to add them automatically into getInstances list. I don't know how to fix it :(
  OpeningBracket
  ClosingBracket
  Plus
  Minus
  Mul
  Div
  Number
}

/**
  * The class of token types which corresponding to arithmetic operators (e.g. +, -, *, /, etc).
  */
sealed trait Operator extends TokenType {
  /**
    * The operator priority.
    */
  val priority: Int

  /**
    * True if the operator is left-associative, false if it's right-associative
    */
  val isLeftAssoc: Boolean
}

/**
  * The token type corresponding to symbol '('
  */
case object OpeningBracket extends TokenType {
  override val regex: Regex = raw"\(".r
}

/**
  * The token type corresponding to symbol ')'
  */
case object ClosingBracket extends TokenType {
  override val regex: Regex = raw"\)".r
}

/**
  * The token type corresponding to symbol '+'
  */
case object Plus extends Operator {
  override val regex: Regex = raw"\+".r
  override val priority = 1
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '-'
  */
case object Minus extends Operator {
  override val regex: Regex = "-".r
  override val priority = 1
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '*'
  */
case object Mul extends Operator {
  override val regex: Regex = raw"\*".r
  override val priority = 2
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '*'
  */
case object Div extends Operator {
  override val regex: Regex = "/".r
  override val priority = 2
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to real positive number (e.g. 4, 1.5, 18, .001, etc)
  */
case object Number extends TokenType {
  override val regex: Regex = raw"(\d*\.)?(\d)+".r
}