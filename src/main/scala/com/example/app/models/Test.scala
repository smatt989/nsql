package com.example.app.models

/**
  * Created by matt on 3/13/18.
  */
object Test {

  val WHAT_TOKEN = Token("what", 0)
  val HP_TOKEN = Token("HP", 3)
  val JOBS_TOKEN = Token("jobs", 4)
  val UNIX_TOKEN = Token("Unix", 7)
  val SYSTEM_TOKEN = Token("system", 8)

  val tokens = Seq(WHAT_TOKEN, HP_TOKEN, JOBS_TOKEN, UNIX_TOKEN, SYSTEM_TOKEN)

  val JOB_RELATION = RelationElement("Job")
  val CITY_RELATION = RelationElement("City")

  val DESCRIPTION_ATTRIBUTE = AttributeElement("Description", JOB_RELATION, isPrimaryKey = true)
  val PLATFORM_ATTRIBUTE = AttributeElement("Platform", JOB_RELATION)
  val COMPANY_ATTRIBUTE = AttributeElement("Company", JOB_RELATION)
  val CITY_NAME_ATTRIBUTE = AttributeElement("Name", CITY_RELATION, isPrimaryKey = true)

  val DEVELOPER_VALUE = ValueElement("Developer", DESCRIPTION_ATTRIBUTE)
  val SYSTEM_ADMIN_VALUE = ValueElement("System Admin", DESCRIPTION_ATTRIBUTE)
  val HP_PLATFORM_VALUE = ValueElement("HP", PLATFORM_ATTRIBUTE)
  val UNIX_VALUE = ValueElement("Unix", PLATFORM_ATTRIBUTE)
  val STRATIFY_VALUE = ValueElement("Stratify", COMPANY_ATTRIBUTE)
  val HP_COMPANY_VALUE = ValueElement("HP", COMPANY_ATTRIBUTE)

  val WHAT_DESCRIPTION_VALUE = ValueElement("What", DESCRIPTION_ATTRIBUTE)
  val WHAT_PLATFORM_VALUE = ValueElement("What", PLATFORM_ATTRIBUTE)
  val WHAT_COMPANY_VALUE = ValueElement("What", COMPANY_ATTRIBUTE)
  val WHAT_CITY_NAME_VALUE = ValueElement("What", CITY_NAME_ATTRIBUTE)

  val elements = Seq(JOB_RELATION, DESCRIPTION_ATTRIBUTE, PLATFORM_ATTRIBUTE, COMPANY_ATTRIBUTE, DEVELOPER_VALUE, SYSTEM_ADMIN_VALUE,
    HP_PLATFORM_VALUE, UNIX_VALUE, STRATIFY_VALUE, HP_COMPANY_VALUE, WHAT_DESCRIPTION_VALUE, WHAT_PLATFORM_VALUE, WHAT_COMPANY_VALUE,
    CITY_RELATION, CITY_NAME_ATTRIBUTE, WHAT_CITY_NAME_VALUE)

  val attachments = Seq(Attachment(WHAT_TOKEN, JOBS_TOKEN), Attachment(HP_TOKEN, JOBS_TOKEN), Attachment(UNIX_TOKEN, SYSTEM_TOKEN))

  val testLexiconMapping = Map(
    HP_TOKEN -> Seq(HP_PLATFORM_VALUE, HP_COMPANY_VALUE),
    UNIX_TOKEN -> Seq(UNIX_VALUE),
    SYSTEM_TOKEN -> Seq(PLATFORM_ATTRIBUTE),
    JOBS_TOKEN -> Seq(JOB_RELATION),
    WHAT_TOKEN -> Seq(WHAT_DESCRIPTION_VALUE, WHAT_PLATFORM_VALUE, WHAT_COMPANY_VALUE)
  )

  val textLexiconMappingBigger = Map(
    HP_TOKEN -> Seq(HP_PLATFORM_VALUE, HP_COMPANY_VALUE),
    UNIX_TOKEN -> Seq(UNIX_VALUE),
    SYSTEM_TOKEN -> Seq(PLATFORM_ATTRIBUTE),
    JOBS_TOKEN -> Seq(JOB_RELATION),
    WHAT_TOKEN -> Seq(WHAT_DESCRIPTION_VALUE, WHAT_PLATFORM_VALUE, WHAT_COMPANY_VALUE, WHAT_CITY_NAME_VALUE)
  )

  val lexicon = Lexicon(elements, Lexicon.STOP_WORDS, Lexicon.WH_WORDS, testLexiconMapping)

  def run = {

    val matches = Matcher.matcher(lexicon, attachments, tokens)

    matches.foreach(m => {
      RAVGraph.printRAVGraph(m)
    })
  }

}
