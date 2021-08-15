
package com.exabeam.rgregory.workshop.validation.task

import cats.data.ValidatedNec

object JsonEntityValidation extends App {

  sealed trait ValidationError
  // entity cannot be parsed
  case class InvalidJson(error: String) extends ValidationError
  // required field is not present in json
  case class MissingField(field: String) extends ValidationError
  // field type is different or requirement is not satisfied, e.g. email is not valid, timestamp is negative
  case class InvalidField(field: String) extends ValidationError

  // Send reminder to a given email at a specific time
  case class Reminder(email: String, reminder: String, remindAt: Long)

  val valid =
    """
      |{
      |  "email": "mister@exabeam.com",
      |  "reminder": "Workshop time!",
      |  "remindAt": 1628781511151
      |}
      |""".stripMargin

  val invalidJson =
    """
      |Not even a JSON
      |""".stripMargin

  val missingEmailField =
    """
      |{
      |  "reminder": "Workshop time!",
      |  "remindAt": 1628781511151
      |}
      |""".stripMargin

  val invalidReminderAndRemindAt =
    """
      |{
      |  "email": "mister@exabeam.com",
      |  "reminder": "",
      |  "remindAt": "1628781511151"
      |}
      |""".stripMargin

  val invalidEmailAndRemindAt =
    """
      |{
      |  "email": "not an email",
      |  "reminder": "Workshop time!",
      |  "remindAt": "1628781511151"
      |}
      |""".stripMargin

  // example of working with Json library
  import play.api.libs.json._
  println(Json.parse(valid).as[JsObject].value("reminder").as[JsString].value)


  // Implement Reminder validation
  //
  // field email must contain '@'
  // field reminder must not be empty
  // field remindAt must be > 0
  def parseReminder(json: String): ValidatedNec[ValidationError, Reminder] = {
    ???
  }

  println(parseReminder(valid))
  println(parseReminder(invalidJson))
  println(parseReminder(missingEmailField))
  println(parseReminder(invalidReminderAndRemindAt))
  println(parseReminder(invalidEmailAndRemindAt))
}
