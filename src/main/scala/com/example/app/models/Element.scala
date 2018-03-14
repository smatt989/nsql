package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
trait Element {
  def name: String
}

case class RelationElement(name: String) extends Element
case class AttributeElement(name: String, relationElement: RelationElement, isPrimaryKey: Boolean = false, foreignKeyTo: Option[RelationElement] = None) extends Element
case class ValueElement(name: String, attributeElement: AttributeElement) extends Element

object Element {

  def isValue(a: Element) = a match {
    case a: ValueElement => true
    case _ => false
  }

  def isAttribute(a: Element) = a match {
    case a: AttributeElement => true
    case _ => false
  }

  def isRelation(a: Element) = a match {
    case a: RelationElement => true
    case _ => false
  }
}