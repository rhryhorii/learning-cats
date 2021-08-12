
package com.exabeam.rgregory.workshop.validation.task

import cats.data.ValidatedNec

object JsonEntityValidation extends App {

  sealed trait ValidationError
  case class InvalidJson(error: String) // entity cannot be parsed
  case class MissingField(field: String) // required field is not present in json
  // field type is different or requirement is not satisfied, e.g. email is not valid, timestamp is negative
  case class InvalidField(field: String)

  // Send reminder to a given email at a specific time
  case class Reminder(email: String, reminder: String, remindAt: Long)

  val validReminder =
    """
      |{
      |  "email": "mister@exabeam.com",
      |  "reminder": "Workshop time!",
      |  "remindAt": 1628781511151
      |}
      |""".stripMargin

  val invalidJsonReminder =
    """
      |Not even a JSON
      |""".stripMargin

  val missingFieldReminder =
    """
      |{
      |  "reminder": "Workshop time!",
      |  "remindAt": 1628781511151
      |}
      |""".stripMargin

  val wrongRemindAtTypeReminder =
    """
      |{
      |  "email": "mister@exabeam.com",
      |  "reminder": "Workshop time!",
      |  "remindAt": "1628781511151"
      |}
      |""".stripMargin

  val invalidFieldConstraintReminder =
    """
      |{
      |  "email": "not an email",
      |  "reminder": "Workshop time!",
      |  "remindAt": "1628781511151"
      |}
      |""".stripMargin

  // example of working with Json library
  import play.api.libs.json._
  println(Json.parse(validReminder).as[JsObject].value("reminder").as[JsString].value)

  // TODO implement
  def parseReminder(json: String): ValidatedNec[ValidationError, Reminder] = ???

  println(parseReminder(validReminder))
  println(parseReminder(invalidJsonReminder))
  println(parseReminder(missingFieldReminder))
  println(parseReminder(wrongRemindAtTypeReminder))
  println(parseReminder(invalidFieldConstraintReminder))
}
