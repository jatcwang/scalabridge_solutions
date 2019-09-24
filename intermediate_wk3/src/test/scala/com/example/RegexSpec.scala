package com.example

import org.scalatest.{Matchers, WordSpec}
import com.example.App.Regex._

class RegexSpec extends WordSpec with Matchers {
  "Literal should work" in {
    Literal("hello").parse("helloworld") shouldBe Some("hello")
  }
  "Repeat should work" in {
    Repeat(Literal("hello")).parse("hellohelloworld") shouldBe Some("hellohello")
  }
  "Sequence should work" in {
    val reg = Sequence(Repeat(Literal("hello")), Literal("world"))
    reg.parse("hellohellohelloworld") shouldBe Some("hellohellohelloworld")
    reg.parse("hellonope") shouldBe None
  }
  "Alternation should work" in {
    val reg = Alternation(Sequence(Literal("hello"), Literal("world")), Literal("nope"))
    reg.parse("helloworld") shouldBe Some("helloworld")
    reg.parse("nope") shouldBe Some("nope")
    reg.parse("another") shouldBe None
  }
}
