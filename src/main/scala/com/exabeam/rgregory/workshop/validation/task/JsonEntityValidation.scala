
package com.exabeam.rgregory.workshop.validation.task

import scala.util.Try
import play.api.libs.json._
import cats.implicits._
import cats.data.{Validated, ValidatedNec}

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

  def validateJson(json: String): ValidatedNec[ValidationError, JsObject] = {
    Validated
      .fromTry(Try {Json.parse(json).as[JsObject]})
      .leftMap(e => InvalidJson(e.getMessage.trim))
      .toValidatedNec
  }

  def validateField(jsObject: JsObject, field: String): Validated[MissingField, JsValue] = {
    Validated.fromOption(jsObject.value.get(field), MissingField(field))
  }

  def validateValue[T: Reads](jsValue: JsValue, condition: T => Boolean): Validated[InvalidField, T] = {
    Validated
      .fromTry(
        for {
          v <- Try { jsValue.as[T] } if condition(v)
        } yield v
      ).leftMap(e => InvalidField(e.getMessage))
  }

  // field email must contain '@'
  def validateEmail(jsObject: JsObject): ValidatedNec[ValidationError, String] = {
    validateField(jsObject, "email")
      .andThen(value => validateValue[String](value, _ contains '@'))
      .toValidatedNec
  }

  // field reminder must not be empty
  def validateReminder(jsObject: JsObject): ValidatedNec[ValidationError, String] = {
    validateField(jsObject, "reminder")
      .andThen(value => validateValue[String](value, _.nonEmpty))
      .toValidatedNec
  }

  // field remindAt must be > 0
  def validateRemindAt(jsObject: JsObject): ValidatedNec[ValidationError, Long] = {
    validateField(jsObject, "remindAt")
      .andThen(value => validateValue[Long](value, _ > 0))
      .toValidatedNec
  }

  def parseReminder(json: String): ValidatedNec[ValidationError, Reminder] = {
    validateJson(json).andThen(valid => (
      validateEmail(valid),
      validateReminder(valid),
      validateRemindAt(valid)
      ).mapN(Reminder.apply)
    )
  }

  println(parseReminder(valid))
  println(parseReminder(invalidJson))
  println(parseReminder(missingEmailField))
  println(parseReminder(invalidReminderAndRemindAt))
  println(parseReminder(invalidEmailAndRemindAt))
}
