package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
class Matcher {

}

object Matcher {


  def matcher(lexicon: Lexicon, attachments: Seq[Attachment], tokens: Seq[Token]) = {

    val ravEdges = constructRAVEdgesFromTokens(tokens.head, lexicon, tokens.tail, Seq[Map[Token, Element]](Map()))

    val ravGraphs = ravEdges.map(rav => RAVGraph(rav, attachments))

    ravGraphs.filter(_.isValidMaxFlow)
  }

  // grabs every possible subgraph given tokens
  def constructRAVEdgesFromTokens(currentToken: Token, lexicon: Lexicon, leftoverTokens: Seq[Token], edgeMaps: Seq[Map[Token, Element]]): Seq[Map[Token, Element]] = {

    val elements = lexicon.elementsByToken(currentToken)

    val newEdges = elements.flatMap(element => {
      edgeMaps.map(edgeMap => {
        edgeMap ++ Map(currentToken -> element)
      })
    })

    if(leftoverTokens.size > 0){
      constructRAVEdgesFromTokens(leftoverTokens.head, lexicon, leftoverTokens.tail, newEdges)
    } else
      newEdges

  }
}