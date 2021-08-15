
package com.exabeam.rgregory.workshop.validation.task

import cats.{Semigroup, SemigroupK}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._

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
    import cats.implicits._

    implicit def necSemigroup: Semigroup[NonEmptyChain[ValidationError]] =
      SemigroupK[NonEmptyChain].algebra[ValidationError]

    def validateEmail(jsObject: JsObject): ValidatedNec[ValidationError, String] =
      jsObject.value.get("email")
        .map { field =>
          Validated.catchOnly[Exception] { field.as[JsString].value }
            .leftMap(_ => InvalidField("email"))
            .andThen { email =>
              Validated.cond(email.contains("@"), email, InvalidField("email"))
            }
            .toValidatedNec
        }.getOrElse(MissingField("email").invalidNec)


    def validateReminder(jsObject: JsObject): ValidatedNec[ValidationError, String] =
      jsObject.value.get("reminder")
        .map { field =>
          Validated.catchOnly[Exception] { field.as[JsString].value }
            .leftMap(_ => InvalidField("reminder"))
            .andThen { reminder =>
              Validated.cond(reminder.nonEmpty, reminder, InvalidField("reminder"))
            }
            .toValidatedNec
        }.getOrElse(MissingField("reminder").invalidNec)

    def validateRemindAt(jsObject: JsObject): ValidatedNec[ValidationError, Long] =
      jsObject.value.get("remindAt")
        .map { field =>
          Validated.catchOnly[Exception] { field.as[JsNumber].value.longValue }
            .leftMap(_ => InvalidField("remindAt"))
            .andThen { remindAt =>
              Validated.cond(remindAt > 0, remindAt, InvalidField("remindAt"))
            }
            .toValidatedNec
        }.getOrElse(MissingField("remindAt").invalidNec)

    Validated.catchOnly[Throwable] {
      Json.parse(json).as[JsObject]
    }
      .leftMap(e => InvalidJson(e.getMessage))
      .toValidatedNec[ValidationError, JsObject]
      .andThen { jsObject =>
        (validateEmail(jsObject), validateReminder(jsObject), validateRemindAt(jsObject))
          .mapN {
            case (email, reminder, remindAt) =>
              Reminder(email, reminder, remindAt)
          }
      }
  }

  println(parseReminder(valid))
  println(parseReminder(invalidJson))
  println(parseReminder(missingEmailField))
  println(parseReminder(invalidReminderAndRemindAt))
  println(parseReminder(invalidEmailAndRemindAt))
}
