package ru.spbau.kaysin.parsing

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
  OpeningBracketType
  ClosingBracketType
  PlusType
  MinusType
  MulType
  DivType
  NumberType
}

/**
  * The class of token types which corresponding to arithmetic operators (e.g. +, -, *, /, etc).
  */
sealed trait OperatorType extends TokenType {
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
case object OpeningBracketType extends TokenType {
  override val regex: Regex = raw"\(".r
}

/**
  * The token type corresponding to symbol ')'
  */
case object ClosingBracketType extends TokenType {
  override val regex: Regex = raw"\)".r
}

/**
  * The token type corresponding to symbol '+'
  */
case object PlusType extends OperatorType {
  override val regex: Regex = raw"\+".r
  override val priority = 1
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '-'
  */
case object MinusType extends OperatorType {
  override val regex: Regex = "-".r
  override val priority = 1
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '*'
  */
case object MulType extends OperatorType {
  override val regex: Regex = raw"\*".r
  override val priority = 2
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to symbol '*'
  */
case object DivType extends OperatorType {
  override val regex: Regex = "/".r
  override val priority = 2
  override val isLeftAssoc = true
}

/**
  * The token type corresponding to real positive number (e.g. 4, 1.5, 18, .001, etc)
  */
case object NumberType extends TokenType {
  override val regex: Regex = raw"(\d*\.)?(\d)+".r
}