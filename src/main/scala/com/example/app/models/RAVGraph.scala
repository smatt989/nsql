package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
case class RAVGraph(tokenToElementMap: Map[Token, Element], attachments: Seq[Attachment]) {
  //TODO: MUST ENSURE ALL TOKENS ARE MAPPED

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

    //TODO: THIS IS WRONG. EXAMPLE: "What states border states that border New York?"
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

  //TODO: HOW TO RELIABLY GET FOCUS ELEMENT (WITHOUT WH-TOKEN FOR EXAMPLE)?
  def findFocusTokens(tokens: Seq[Token], lexicon: Lexicon) =
    tokens.filter(lexicon.isAWhToken)

  def findFocusElement(token: Token, graph: RAVGraph) = {
    val focusTokenElement = graph.tokenToElementMap(token)
    /*For readability, WH tokens are assined as Values.
      We can determine what the focus element is by inspecting the attribute it is associated with:
      if it is associated with a primary attribute, it is referring to the relation,
      if it is associated with a non-primary attribute, it is referring to the attribute.*/
    //IS THIS LEGIT?

    focusTokenElement match {
      case e: ValueElement if e.attributeElement.isPrimaryKey => e.attributeElement.relationElement
      case e: ValueElement => e.attributeElement
    }
  }

  def findFocusWhToken(attachment: Attachment, lexicon: Lexicon) =
    if(lexicon.isAWhToken(attachment.a))
      attachment.a
    else
      attachment.b

  def relationIsMentioned(relationElement: RelationElement, ravGraph: RAVGraph, lexicon: Lexicon) = {
    ravGraph.relationTokens.toSeq.map(_._2).contains(relationElement) ||
      //need to filter out the value WH tokens (because currently testing it)
      ravGraph.valueTokens.toSeq.filterNot(a => lexicon.isAWhToken(a._1)).map(_._2.attributeElement).filter(_.isPrimaryKey).map(_.relationElement).contains(relationElement)
  }

  def validMapping(ravGraph: RAVGraph, lexicon: Lexicon) = {
    RAVGraph.printRAVGraph(ravGraph)
    if(existsAUniqueFocusElement(ravGraph, lexicon)){
      if(relationFocusMarkersAreOnMentionedRelations(ravGraph, lexicon)){
        if(attributeFocusMarkersAreOnMentionedRootedAttributes(ravGraph, lexicon)){
          if(nonFocusAttributesSatisfyCorrespondences(ravGraph, lexicon)) {
            if(valueElementsSatisfyCorrespondences(ravGraph, lexicon)) {
              if(allMentionedElementsAreConnected(ravGraph, lexicon)){
                println("SUCCEEDED")
                true
              } else {
                println("failed test 6")
                false
              }
            } else {
              println("failed test 5")
              false
            }
          } else {
            println("failed test 4")
            false
          }
        } else {
          println("failed test 3")
          false
        }
      } else {
        println("failed test 2")
        false
      }
    } else {
      println("failed test 1")
      false
    }
    //existsAUniqueFocusElement(ravGraph, lexicon) &&
    //  relationFocusMarkersAreOnMentionedRelations(ravGraph, lexicon) &&
    //  attributeFocusMarkersAreOnMentionedRootedAttributes(ravGraph, lexicon) &&
    //  nonFocusAttributesSatisfyCorrespondences(ravGraph, lexicon) &&
    //  valueElementsSatisfyCorrespondences(ravGraph, lexicon) &&
    //  allMentionedElementsAreConnected(ravGraph, lexicon)
  }



  // 1. THERE EXISTS A UNIQUE FOCUS ELEMENT
  /* There always exists a unique focus element that is the attribute or relation upon which the question is
  seeking information.*/
  //adding attachment requirement here... not sure this is the right interpretation...
  def existsAUniqueFocusElement(ravGraph: RAVGraph, lexicon: Lexicon) = {
    val focusTokens = findFocusTokens(ravGraph.tokenToElementMap.keys.toSeq, lexicon)

    if(focusTokens.size == 1){
      val focusToken = focusTokens.head

      val focusElement = findFocusElement(focusToken, ravGraph)

      val attachments = ravGraph.attachments.filter(_.tokenInAttachment(focusToken))

      attachments.forall(attachment => {
        val otherToken = attachment.pairedWith(focusToken).get
        val otherElement = ravGraph.tokenToElementMap(otherToken)
        val focusTokenElement = ravGraph.tokenToElementMap(focusToken).asInstanceOf[ValueElement]

        focusTokenElement.attributeElement == otherElement || focusTokenElement.attributeElement.relationElement == otherElement
      }) && (Element.isRelation(focusElement) || Element.isAttribute(focusElement)) // this last part maybe redundant??
    } else {
      false
    }
  }

  // 2. ALL RELATION FOCUS MARKERS ARE ON MENTIONED RELATIONS
  /* If a relation is a focus, then it must be mentioned.  A relation is mentioned if either it is explicitly named
  in the question or if a primary value of the relation is within the question*/

  def relationFocusMarkersAreOnMentionedRelations(ravGraph: RAVGraph, lexicon: Lexicon) = {
    //Assumes first condition was passed
    val focusToken = findFocusTokens(ravGraph.tokenToElementMap.keys.toSeq, lexicon).head

    val focusElement = findFocusElement(focusToken, ravGraph)

    if(Element.isRelation(focusElement)){
      relationIsMentioned(focusElement.asInstanceOf[RelationElement], ravGraph, lexicon)
    } else
      true
  }

  // 3. ALL ATTRIBUTE FOCUS MARKERS ARE ON MENTIONED, ROOTED ATTRIBUTES
  /* An attribute focus marker (e.g. 'what') must not only explicitly match an attribute (e.g. 'population'), but
  such an attribute must also be rooted.  An attribute is rooted if the relation of the attribute is mentioned.*/

  def attributeFocusMarkersAreOnMentionedRootedAttributes(ravGraph: RAVGraph, lexicon: Lexicon) = {
    //Assumes first condition was passed
    val focusToken = findFocusTokens(ravGraph.tokenToElementMap.keys.toSeq, lexicon).head

    val focusElement = findFocusElement(focusToken, ravGraph)

    if(Element.isAttribute(focusElement)){
      val tokenAttachments = ravGraph.attachments.filter(_.tokenInAttachment(focusToken))
      if(tokenAttachments.size == 1){
        val tokenAttachment = tokenAttachments.head

        val (attachmentWhToken, otherAttachmentToken) = (tokenAttachment.a, tokenAttachment.b) match {
          case (wh, a) if lexicon.isAWhToken(wh) => (wh, a)
          case (a, wh) if lexicon.isAWhToken(wh) => (wh, a)
        }

        val whTokenElement = ravGraph.tokenToElementMap(attachmentWhToken)
        val otherTokenElement = ravGraph.tokenToElementMap(otherAttachmentToken)

        val whTokenExplicitlyMatchesAnAttribute = otherTokenElement match {
          case o: AttributeElement => o == whTokenElement.asInstanceOf[ValueElement].attributeElement
          case _ => false
        }

        val relationOfAttributeIsMentioned = otherTokenElement match {
          case o: AttributeElement => relationIsMentioned(o.relationElement, ravGraph, lexicon)
          case _ => false
        }

        whTokenExplicitlyMatchesAnAttribute && relationOfAttributeIsMentioned

      } else
        false
    } else
      true
  }

  // 4. NON-FOCUS ATTRIBUTES SATISFY CORRESPONDENCES
  /* Unless they are the focus, attributes must pair with a value, or, in the case that the attribute is a
  foreign key (e.g. 'cities *in* the *state*'), the attribute must pair with the relation or primary value
  of the foreign key (e.g. 'cities *in Ohio*')*/

  //MAYBE 'IN' SHOULD ALWAYS BE ATTEMPTED TO ALL PRIMARY KEY AND FOREIGN KEY ATTRIBUTES
  //FOR NOW PRETEND 'IN' IS JUST A STOP WORD... see what happens... maybe a problem for connected graph?
  //As a result skipping the foreign key part of the above... see what happens...

  def nonFocusAttributesSatisfyCorrespondences(ravGraph: RAVGraph, lexicon: Lexicon) = {

    ravGraph.attributeTokens.toSeq.forall{ case (attributeToken, attributeElement) => {
      val attachments = ravGraph.attachments.filter(_.tokenInAttachment(attributeToken))
      if(attachments.size > 0) {
        attachments.exists(a => {
          val pairedToken = a.pairedWith(attributeToken).get
          val pairedTokenElement = ravGraph.tokenToElementMap(pairedToken)

          Element.isValue(pairedTokenElement) && pairedTokenElement.asInstanceOf[ValueElement].attributeElement == attributeElement
        })
      } else
        false
    }}
  }

  // 5. VALUE ELEMENTS SATISFY CORRESPONDENCES
  /* Values are either primary or must be paired with either an attribute, or via ellipsis paired with a relation*/

  def valueElementsSatisfyCorrespondences(ravGraph: RAVGraph, lexicon: Lexicon) = {
    val valueTokens = ravGraph.valueTokens

    valueTokens.forall{case (valueToken, valueElement) => {
      val valueElementIsPrimary = valueElement.attributeElement.isPrimaryKey

      if(valueElementIsPrimary)
        true
      else {
        val attachments = ravGraph.attachments.filter(_.tokenInAttachment(valueToken))

        if (attachments.size > 0) {
          attachments.exists(a => {
            val pairedToken = a.pairedWith(valueToken).get
            val pairedTokenElement = ravGraph.tokenToElementMap(pairedToken)

            Element.isAttribute(pairedTokenElement) && pairedTokenElement == valueElement.attributeElement ||
              Element.isRelation(pairedTokenElement) && pairedTokenElement == valueElement.attributeElement.relationElement
          })
          true
        } else
          false
      }
    }}
  }

  // 6. ALL MENTIONED ELEMENTS ARE CONNECTED
  /* The elements assigned by the mapping must form a connected graph over the underlying database schema */

  def allMentionedElementsAreConnected(ravGraph: RAVGraph, lexicon: Lexicon) = {
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