package ru.spbau.kaysin.set

import org.scalatest.{FlatSpec, Matchers}

class MultisetTest extends FlatSpec with Matchers {
  "A Multiset" should "be able to contain multiple entries of the same value" in {
    val set = Multiset(Seq(1, 2, 2))
    set.size shouldBe 3
  }

  "A Multiset" should "be able to be empty" in {
    val set = Multiset(Seq.empty)
    set.size shouldBe 0
    set shouldBe Multiset.empty
  }

  "A Multiset filter" should "filter" in {
    val set = Multiset(Seq(1, 2, 3)).filter(_ < 3)
    set shouldBe Multiset(Seq(1, 2))
  }

  "A Multiset map" should "map" in {
    val set = Multiset(Seq(1, 2, 3)).map(_ + 1)
    set shouldBe Multiset(Seq(2, 3, 4))
  }

  "A Multiset flatMap" should "work" in {
    val set = Multiset(Seq(1, 2, 3)).flatMap(x => Seq(x, x))
    set shouldBe Multiset(Seq(1, 2, 3, 1, 2, 3))
  }

  "A Multiset find" should "find the element if it's presented" in {
    Multiset(Seq(1, 2, 3)).find(1) shouldBe Some(1)
  }

  "A Multiset find" should "return None if it isn't presented" in {
    Multiset(Seq(1, 2, 3)).find(4) shouldBe None
  }

  "A Multiset apply" should "return the number of contained elements which equal the argument" in {
    Multiset(Seq(1, 2, 3, 3))(3) shouldBe 2
  }


  "Unapply" should "work" in {
    (Multiset(Seq(1, 2)) match {
      case Multiset(Seq(1, 2)) => true
      case Multiset(Seq(2, 1)) => true
      case _ => false
    }) shouldBe true
  }

  "A Multiset operator :+" should "add a new element to the multiset" in {
    Multiset(Seq(1, 2)) :+ 2 shouldBe Multiset(Seq(1, 2, 2))
  }

  "A Multiset operator ++" should "unite two multisets" in {
//    println((Multiset(Seq(1, 2)) ++ Multiset(Seq(2, 2, 3))).toSeq)
    (Multiset(Seq(1, 2)) ++ Multiset(Seq(2, 2, 3))) shouldBe Multiset(Seq(1, 2, 2, 3))
  }

  "A Multiset method intersect" should "intersect two multisets" in  {
    Multiset(Seq(1, 2, 3, 3, 5, 5)) intersect Multiset(Seq(2, 2, 3, 4, 5, 5, 5)) shouldBe Multiset(Seq(2, 3, 5, 5))
  }

  "For-comprehension syntax" should "work with Multiset" in {
    (for {
      x <- Multiset(Seq(1, 3, 2, 3))
    } yield {
      x
    }) shouldBe Multiset(Seq(1, 3, 2, 3))
  }
}
