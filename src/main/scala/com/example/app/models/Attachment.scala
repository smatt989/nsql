package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
case class Attachment(a: Token, b: Token) {

  def tokenInAttachment(token: Token) =
    a == token || b == token

  def pairedWith(token: Token) =
    if(a == token)
      Some(b)
    else if(b == token)
      Some(a)
    else
      None
}
