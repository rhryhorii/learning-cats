
package com.exabeam.rgregory.workshop.validation.task

import cats.data.{Validated, ValidatedNec}
import cats.implicits._

import scala.util.Try

object JsonEntityValidation extends App {

  type ValidationResult[A] = ValidatedNec[ValidationError, A]

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

  def validateMissingField(reminder: JsObject, field: String): Validated[MissingField, JsValue] = {
    Validated.fromOption(
      reminder.value.get(field),
      MissingField(field)
    )
  }

  def validate[T: Reads](jsValue: JsValue, cond: T => Boolean): Validated[Throwable, T] = {
    Validated.fromTry {
      for {
        value <- Try {
          jsValue.as[T]
        } if cond(value)
      } yield value
    }
  }

  def validateEmail(reminder: JsObject): ValidationResult[String] = {
    validateMissingField(reminder, "email")
      .andThen(jsValue => validate[String](jsValue, _ contains "@").leftMap(_ => InvalidField("email")))
      .toValidatedNec
  }

  def validateReminder(reminder: JsObject): ValidationResult[String] = {
    validateMissingField(reminder, "reminder")
      .andThen(jsValue => validate[String](jsValue, _.nonEmpty).leftMap(_ => InvalidField("reminder")))
      .toValidatedNec
  }

  def validateRemindAt(reminder: JsObject): ValidationResult[Long] = {
    validateMissingField(reminder, "remindAt")
      .andThen(jsValue => validate[Long](jsValue, _ > 0).leftMap(_ => InvalidField("remindAt")))
      .toValidatedNec
  }

  def validateJson(json: String): ValidationResult[JsObject] = {
    Validated
      .fromTry(Try(Json.parse(json).as[JsObject]))
      .leftMap(ex => InvalidJson(ex.getMessage))
      .toValidatedNec
  }

  def validateFieldValues(reminder: JsObject): ValidationResult[Reminder] = {
    (
      validateEmail(reminder),
      validateReminder(reminder),
      validateRemindAt(reminder)
      ).mapN(Reminder.apply)
  }

  // Implement Reminder validation
  //
  // field email must contain '@'
  // field reminder must not be empty
  // field remindAt must be > 0
  def parseReminder(json: String): ValidatedNec[ValidationError, Reminder] = {
    validateJson(json).andThen(validateFieldValues)
  }

  println(parseReminder(valid))
  println(parseReminder(invalidJson))
  println(parseReminder(missingEmailField))
  println(parseReminder(invalidReminderAndRemindAt))
  println(parseReminder(invalidEmailAndRemindAt))
}
