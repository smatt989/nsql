package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
case class Lexicon(elements: Seq[Element], stopWords: Seq[String], whWords: Seq[String], testMapping: Map[Token, Seq[Element]] = Map()) {

  def elementsByToken(token: Token): Seq[Element] = {
    //if(!testMapping.isEmpty)
    //  ???
    //else
      testMapping.getOrElse(token, Nil)
  }

  def isAWhToken(token: Token) =
    whWords.contains(token.name.toLowerCase)

}

object Lexicon {
  val STOP_WORDS = Seq("are", "the", "on", "a", "in", "is", "be", "of", "do", "with", "by", "ha", "has", "have", "some", "that", "there", "and", "all")
  val WH_WORDS = Seq("what", "which", "where", "who", "when")
}