package ru.spbau.kaysin.parsing

import org.scalatest.FunSuite

class TokenizerTest extends FunSuite {

  test("tokenize operators") {
    val string = " + - * / ( ) "
    val expected = Some (List(Token(PlusType,"+"),
                         Token(MinusType, "-"),
                         Token(MulType, "*"),
                         Token(DivType, "/"),
                         Token(OpeningBracketType, "("),
                         Token(ClosingBracketType, ")")))
    val actual = getActual(string)
    assert(actual == expected)
  }

  test("tokenize natural number") {
    testSingleNumber("239")
    testSingleNumber("1")
  }

  test("tokenize pointed number") {
    testSingleNumber("0.1")
    testSingleNumber("42.239")
    testSingleNumber(".42")
  }

  test("tokenize empty string") {
    val string = ""
    val actual = getActual(string)
    val expected = Some(List.empty)
    assert(actual == expected)
  }

  test("failed to tokenize") {
    val string = "foo"
    val actual = getActual(string)
    val expected = None
    assert(actual == expected)
  }

  private def getActual = Tokenizer.tokenize _

  private def testSingleNumber(number: String): Unit = {
    val actual = Tokenizer.tokenize(number)
    val expected = Some(List(Token(NumberType, number)))
    assert(actual == expected)
  }
}
