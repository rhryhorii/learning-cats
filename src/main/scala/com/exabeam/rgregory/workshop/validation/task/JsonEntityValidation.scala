
package com.exabeam.rgregory.workshop.validation.task

import cats.data.NonEmptyChain.one
import cats.data.ValidatedNec
import scala.util.Either.cond

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
    import cats.data._
    import cats.implicits._

    def validate[A: Reads](jsValue: JsValue, fieldName: String, check: A => Boolean): Either[NonEmptyChain[ValidationError], A] = {
      for {
        js <- Either.catchNonFatal(jsValue(fieldName))
          .leftMap(e => one(MissingField(s"Field $fieldName is missing. Exception: $e")))
        v <- Either.catchNonFatal(js.as[A])
          .leftMap(e => one(InvalidField(s"$fieldName: Incorrect field type, $e")))
        r <- cond(check(v), v, one(InvalidField(fieldName)))
      } yield r
    }

    val r = for {
      j <- Either.catchNonFatal(Json.parse(json)).leftMap(e => one(InvalidJson(s"error $e")))
      email = validate[String](j, "email", _ contains "@")
      reminder = validate[String](j, "reminder", _.nonEmpty)
      remindAt = validate[Long](j, "remindAt", _ > 0)
      rez <- (email, reminder, remindAt).parMapN(Reminder)
    } yield rez

    r.toValidated
  }


  println(parseReminder(valid))
  println(parseReminder(invalidJson))
  println(parseReminder(missingEmailField))
  println(parseReminder(invalidReminderAndRemindAt))
  println(parseReminder(invalidEmailAndRemindAt))
}
