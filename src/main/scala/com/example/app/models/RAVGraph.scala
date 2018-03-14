package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
case class RAVGraph(tokenToElementMap: Map[Token, Element], attachments: Seq[Attachment]) {

  lazy val valueTokens = tokenToElementMap.filter(a => Element.isValue(a._2)).mapValues(_.asInstanceOf[ValueElement])
  lazy val attributeTokens = tokenToElementMap.filter(a => Element.isAttribute(a._2)).mapValues(_.asInstanceOf[AttributeElement])
  lazy val relationTokens = tokenToElementMap.filter(a => Element.isRelation(a._2)).mapValues(_.asInstanceOf[RelationElement])


  def isValidMaxFlow = {
    val uniqueAttributes = valueTokens.toSeq.map(value => value._2.attributeElement).distinct

    //this is interesting.  multiple tokens can point to the same attribute element. as a result,
    //this map might contain multiple of the same keys, meaning some information is lost. however
    //the presence of two tokens pointing to the same attribute disqualifies this graph.  this is
    //reflected by the fact that this graph will NOT be deemed valid since the number of attribute
    //tokens will not match the number of explicit attributes.

    val tokensByAttribute = attributeTokens.toSeq.map{case (token, element) => (element -> token) }.toMap

    val (explicitAttributes, implicitAttributes) = uniqueAttributes.partition(a => tokensByAttribute.get(a).isDefined)

    val maxFlow = explicitAttributes.size + implicitAttributes.size

    validateMaxFlow(maxFlow) &&
      validateImplicitAttributes(implicitAttributes) &&
      validateExplicitAttributes(explicitAttributes) &&
      isValidRelationFlow
  }

  //THESE CONDITIONS MUST BE SATISFIED TO HAVE A VALID MAXFLOW SOLUTION
  def isValidRelationFlow = {
    val impliedRelations = (valueTokens.toSeq.map(_._2).map(_.attributeElement.relationElement) ++
      attributeTokens.toSeq.map(_._2.relationElement)).distinct

    //each relation token points to a distinct relation
    //each relation token points to a relation that is implied
    relationTokens.toSeq.map(_._2).distinct.size == relationTokens.size &&
      relationTokens.forall(relation => impliedRelations.contains(relation._2))
  }

  def validateImplicitAttributes(implicitAttributes: Seq[AttributeElement]) =
    valueTokens.size - attributeTokens.size == implicitAttributes.size

  def validateExplicitAttributes(explicitAttributes: Seq[AttributeElement]) =
    attributeTokens.size == explicitAttributes.size

  def validateMaxFlow(maxFlow: Int) =
    maxFlow == valueTokens.size


}

object RAVGraph {

  def validMappings(ravGraph: RAVGraph) = {

  }


  def findFocusAttachments(ravGraph: RAVGraph, lexicon: Lexicon) =
    ravGraph.attachments.filter(attachment => lexicon.isAWhToken(attachment.a) || lexicon.isAWhToken(attachment.b))

  def findFocusToken(attachment: Attachment, lexicon: Lexicon) =
    if(lexicon.isAWhToken(attachment.a))
      attachment.b
    else
      attachment.a

  // 1. THERE EXISTS A UNIQUE FOCUS ELEMENT
  /* There always exists a unique focus element that is the attribute or relation upon which the question is
  seeking information.*/
  def existsAUniqueFocusElement(ravGraph: RAVGraph, lexicon: Lexicon) = {
    val focusAttachments = findFocusAttachments(ravGraph, lexicon)
    if(focusAttachments.size == 1){
      val focusAttachment = focusAttachments.head
      val focus = findFocusToken(focusAttachment, lexicon)

      val dbElement = ravGraph.tokenToElementMap(focus)

      Element.isRelation(dbElement) || Element.isAttribute(dbElement)
    } else {
      false
    }
  }

  // 2. ALL RELATION FOCUS MARKERS ARE ON MENTIONED RELATIONS
  /* If a relation is a focus, then it must be mentioned.  A relation is mentioned if either it is explicitly named
  in the question or if a primary value of the relation is within the question*/

  def relationFocusMarkersAreOnMentionedRelations(ravGraph: RAVGraph, lexicon: Lexicon) = {
    //Assumes first condition was passed
    val focusAttachment = findFocusAttachments(ravGraph, lexicon).head
    val focusToken = findFocusToken(focusAttachment, lexicon)

    val focusElement = ravGraph.tokenToElementMap(focusToken)

    if(Element.isRelation(focusElement)){

    } else
      true
  }




  def printRAVGraph(m: RAVGraph) = {
    println("\n\nONE GRAPH: ")
    println("Relation Mappings:")
    m.relationTokens.foreach(r => {
      println(r._1.name+" -> "+r._2.name)
    })
    println("Attribute Mappings:")
    m.attributeTokens.foreach(r => {
      println(r._1.name+" -> "+r._2.name+": "+r._2.relationElement.name)
    })
    println("Value Mappings:")
    m.valueTokens.foreach(r => {
      println(r._1.name+" -> "+r._2.name+": "+r._2.attributeElement.name)
    })
  }
}