package org.alfiler

import cats.data.Reader

case class Db (usernames: Map[Int,String],passwords: Map[String,String])

object ReaderDB {

  type DbReader[A] = Reader[Db,A]

  def findUsername(userId:Int):DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
  def checkPassword(username:String, password: String):DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))
  def checkLogin(userId:Int, password:String):DbReader[Boolean] =
    for {
      userName <- findUsername(userId)
      passwordCorrect <- checkPassword(userName.getOrElse(""),password)
    } yield passwordCorrect

  def main(args: Array[String]): Unit = {

    val a = Db(Map(1->"Alfonso"), Map("Alfonso" -> "yepe"))

    println(checkLogin(3, "yepa").run(a))
  }
}
