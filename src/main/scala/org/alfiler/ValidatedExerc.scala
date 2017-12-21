package org.alfiler

import cats.data.Validated

import scala.language.higherKinds

case class User(name:String, age:Int)

object ValidatedExerc {

  import cats.syntax.either._

  type FastFail[A] = Either[List[String],A]
  type SlowFail[A] = Validated[List[String], A]

  def main(args: Array[String]): Unit = {
    println(getUser(Map("name" -> "Alfonso", "age"-> "32")))
  }

  def getValue(name:String, map:Map[String,String]):FastFail[String] =
    Either.fromOption(map.get(name), List("Name not found"))

  def parseInt(value:String):FastFail[Int] =
    Either.catchOnly[NumberFormatException](value.toInt).leftMap(_ => List("Cannot be parsed to Int"))

  def nonBlanck(value:String):FastFail[String] =
    Either.cond(value.nonEmpty,value,List("Name is empty"))

  def nonNegative(value:Int):FastFail[Int] =
    Either.cond(value > 0,value,List("Age has to be positive"))

  def readName(map:Map[String,String]):FastFail[String] = {
    for {
      name <- getValue("name",map)
      nonBlanckName <- nonBlanck(name)
    } yield nonBlanckName
  }

  def readAge(map:Map[String,String]):FastFail[Int] = {
    for {
      age <- getValue("age",map)
      ageInt <- parseInt(age)
      positiveAge <- nonNegative(ageInt)
    } yield positiveAge
  }


  def getUser(dataMap:Map[String,String]):SlowFail[User] = {
    import cats.instances.list._
    import cats.syntax.apply._

    val validName:SlowFail[String] = readName(dataMap).toValidated

    val validAge:SlowFail[Int] = readAge(dataMap).toValidated
    (
      validName,
      validAge
    ).mapN(User.apply)
  }


  def doSomething() = {
    import cats.instances.option._
    import cats.syntax.apply._
    (Option("hola"),Option(23342)).mapN((a,b) => User.apply(a,b))
  }
}
