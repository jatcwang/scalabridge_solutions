package com.example

import scala.annotation.tailrec

object App {

  def main(args: Array[String]): Unit = {
    println("Hello com.example.intermediate_wk3!")
  }

  sealed trait Regex {
    import Regex._
    def parse(input: String): Option[String] = {
      this match {
        case Literal(targetStr)         => if (input.startsWith(targetStr)) Some(targetStr) else None
        case Alternation(first, fallback) => {
          val firstResult = first.parse(input)
          if (firstResult.nonEmpty) firstResult
          else fallback.parse(input)
        }
        case Sequence(first, second)    => {
          val firstResult = first.parse(input)
          firstResult match {
            case Some(firstMatchedStr) => {
              val newInput = input.drop(firstMatchedStr.length)
              second.parse(newInput) match {
                case Some(secondMatchedStr) => Some(firstMatchedStr + secondMatchedStr)
                case None => None
              }
            }
            case None => None
          }
        }
        case Repeat(regex)              => {
          @tailrec
          def inner(currentlyMatchedStr: String, thisInput: String): Option[String] = {
            regex.parse(thisInput) match {
              case Some(matchedStr) => {
                val newAccum = currentlyMatchedStr + matchedStr
                inner(newAccum, thisInput.drop(matchedStr.length))
              }
              case None => if (currentlyMatchedStr.nonEmpty) Some(currentlyMatchedStr) else None
            }
          }
          inner("", input)
        }
      }
    }
  }

  object Regex {

    case class Literal(targetStr: String) extends Regex {}

    case class Alternation(first: Regex, second: Regex) extends Regex {}

    case class Sequence(first: Regex, second: Regex) extends Regex {}

    case class Repeat(regex: Regex) extends Regex {}

  }


}
