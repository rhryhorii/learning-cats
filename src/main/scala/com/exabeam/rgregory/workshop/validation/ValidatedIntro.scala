
package com.exabeam.rgregory.workshop.validation

import cats.{Semigroup, SemigroupK}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}

import scala.util.Try

object ValidatedIntro extends App {

  /*

  sealed abstract class Validated[+E, +A] extends Product with Serializable ...

  */

  // 1. domain entity definition
  case class User(username: String, password: String, age: Int, fullName: String)

  // 2. entity creation request
  case class CreateUserRequest(username: String, password: String, age: Int, fullName: String)

  // 3. validation functions

  def validateUsername(username: String): Try[String] = Try {
    val regex = """^[a-z0-9_]+$"""
    if (username.matches(regex)) username
    else throw new IllegalArgumentException(s"invalid username, doesn't match $regex")
  }

  def validatePassword(password: String): Try[String] = Try {
    val regex = """^(?=\S*[a-z])(?=\S*[A-Z])(?=\S*\d)(?=\S*[^\w\s])\S{8,}$"""
    if (password.matches(regex)) password
    else throw new IllegalArgumentException(s"invalid password, doesn't match $regex")
  }

  def validateAge(age: Int): Try[Int] = Try {
    if (age >= 16) age
    else throw new IllegalArgumentException("invalid age, less then 16")
  }

  def validateFullName(fullName: String): Try[String] = Try {
    val regex = """^[a-zA-Z]{1,}\s[a-zA-Z]{1,}$"""
    if (fullName.matches(regex)) fullName
    else throw new IllegalArgumentException(s"invalid fullName, doesn't match $regex")
  }

  {
    // !!!PROBLEM!!! How to report all errors simultaneously?
    // have you written something like this before
    def validateUser(request: CreateUserRequest): Try[User] =
      for {
        username <- validateUsername(request.username)
        password <- validatePassword(request.password)
        age <- validateAge(request.age)
        fullName <- validateFullName(request.fullName)
      } yield User(username, password, age, fullName)

    val invalidUser = CreateUserRequest(
      username = "Tom", // should be lower cased
      password = "123456", // need stronger password
      age = 15, // wait one more year
      fullName = "TomBrown" // missing space
    )

    // single error at a time
    validateUser(invalidUser) //: Failure[IllegalArgumentException]
  }

  // 4. Validated data class

  Validated.valid("gregory"): Validated[Nothing, String]
  Validated.invalid(new IllegalArgumentException("invalid username")): Validated[Throwable, Nothing]
  // convert exception to invalid
  Validated.catchOnly[NumberFormatException] { "foo".toInt }: Validated[NumberFormatException, Int] // invalid

  Validated.fromEither(Right("abc"): Either[Throwable, String]): Validated[Throwable, String]
  Validated.fromTry(Try {"foo".toInt }): Validated[Throwable, Int]
  Validated.fromOption(
    Some("abc"),
    ifNone = new IllegalArgumentException("missing value")): Validated[Throwable, String]

  Validated.cond(
    test = 4 / 2 == 2,
    "two",
    new IllegalArgumentException("empty")
  ): Validated[Throwable, String]

  {
    import cats.implicits._
    val v1 = "error".invalid[Int]
    val v2 = 123.valid[String]
  }

  // error list is represented as NonEmptyChain[E]
  // type ValidatedNec[+E, +A] = Validated[NonEmptyChain[E], A]
  Validated.validNec[Throwable, String]("result"): ValidatedNec[Throwable, String]
  Validated.invalidNec[Throwable, String](new IllegalArgumentException("wrong")): ValidatedNec[Throwable, String]

  // 5. Error representation
  case class ErrorMessage(msg: String)

  // Semigroup provides a way to combine errors
  /*
  implicit def necSemigroup: Semigroup[NonEmptyChain[ErrorMessage]] = (a, b) => a.append(b)
   */

  // e.g. SemigroupK[F] can produce a Semigroup[F[A]] for any type A
  implicit def necSemigroup: Semigroup[NonEmptyChain[ErrorMessage]] =
    SemigroupK[NonEmptyChain].algebra[ErrorMessage]


  // 6. defined validated for each field
  def validatedUsername(username: String): ValidatedNec[ErrorMessage, String] =
    Validated.fromTry(validateUsername(username))
      .leftMap(e => ErrorMessage(e.getMessage))
      .toValidatedNec

  def validatedPassword(password: String): ValidatedNec[ErrorMessage, String] =
    Validated.fromTry(validatePassword(password))
      .leftMap(e => ErrorMessage(e.getMessage))
      .toValidatedNec

  def validatedAge(age: Int): ValidatedNec[ErrorMessage, Int] =
    Validated.fromTry(validateAge(age))
      .leftMap(e => ErrorMessage(e.getMessage))
      .toValidatedNec

  def validatedFullName(fullName: String): ValidatedNec[ErrorMessage, String] =
    Validated.fromTry(validateFullName(fullName))
      .leftMap(e => ErrorMessage(e.getMessage))
      .toValidatedNec


  // 7. compose validated

  def validatedUser1(request: CreateUserRequest): ValidatedNec[ErrorMessage, User] = {
    import cats.implicits._
    // requires implicit Semigroupal[F]
    (
      validatedUsername(request.username),
      validatedPassword(request.password),
      validatedAge(request.age),
      validatedFullName(request.fullName)
    )
      .mapN { case (username, password, age, fullName) =>
        User(username, password, age, fullName)
      }
  }

  println("validated user 1 (invalid)")
  println(
    validatedUser1(
      CreateUserRequest(
        username = "tom",
        password = "123456", // need stronger password
        age = 15, // wait one more year
        fullName = "TomBrown" // missing space
      )
    )
  )
  //Invalid(
  // Chain(
  //  ErrorMessage(invalid password, doesn't match ^(?=\S*[a-z])(?=\S*[A-Z])(?=\S*\d)(?=\S*[^\w\s])\S{8,}$),
  //  ErrorMessage(invalid age, less then 16),
  //  ErrorMessage(invalid fullName, doesn't match ^[a-zA-Z]{1,}\s[a-zA-Z]{1,}$)
  // )
  //)

  println("validated user 1 (valid)")
  println(
    validatedUser1(
      CreateUserRequest(
        username = "tom",
        password = "%^&Vcxb8!IUBcds",
        age = 30,
        fullName = "Tom Brown"
      )
    )
  )
  // Valid(User(tom,%^&Vcxb8!IUBcds,30,Tom Brown))



  def validatedUser2(request: CreateUserRequest): ValidatedNec[ErrorMessage, User] = {
    type F[E] = ({type T = ValidatedNec[ErrorMessage, E]})#T

    import cats._

    // can use Applicative.apply[F]
    // Apply is Applicative without pure
    Apply[F].map4(
      validatedUsername(request.username),
      validatedPassword(request.password),
      validatedAge(request.age),
      validatedFullName(request.fullName)
    ) {
      User.apply
    }
  }

  println("validated user 2")
  println(
    validatedUser2(
      CreateUserRequest(
        username = "tom",
        password = "123456", // need stronger password
        age = 15, // wait one more year
        fullName = "TomBrown" // missing space
      )
    )
  )
  //Invalid(
  // Chain(
  //  ErrorMessage(invalid password, doesn't match ^(?=\S*[a-z])(?=\S*[A-Z])(?=\S*\d)(?=\S*[^\w\s])\S{8,}$),
  //  ErrorMessage(invalid age, less then 16),
  //  ErrorMessage(invalid fullName, doesn't match ^[a-zA-Z]{1,}\s[a-zA-Z]{1,}$)
  // )
  //)

  // Example of sequential validation with .andThen
  // "chain" Validated, meaning go deeper until first failure. Same as composing Try or Either
  def validatedSequentially(request: CreateUserRequest): ValidatedNec[ErrorMessage, User] =
    validatedUsername(request.username).andThen { username =>
      validatedPassword(request.password).andThen { password =>
        validatedAge(request.age).andThen { age =>
          validatedFullName(request.fullName).map { fullName =>
            (username, password, age, fullName)
          }
        }
      }
    }.map { case (username, password, age, fullName) =>
      User(username, password, age, fullName)
    }

  println("validated user sequentially")
  println(validatedSequentially(
    CreateUserRequest(
      username = "tom",
      password = "123456", // need stronger password
      age = 15, // wait one more year
      fullName = "TomBrown" // missing space
    )
  ))
  // Invalid(Chain(ErrorMessage(invalid password, doesn't match ^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$)))

}
