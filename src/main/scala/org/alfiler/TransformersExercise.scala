package org.alfiler

import cats.data.EitherT

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object TransformersExercise {
  import cats.implicits._
  import scala.concurrent.ExecutionContext.Implicits.global


  type Response[A] = Future[Either[String,A]]

  type ResponseT[A] = EitherT[Future,String,A]

  val powerLevels = Map (
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot:String):ResponseT[Int] = powerLevels.get(autobot).fold(s"Error: No encontrado $autobot".asLeft[Int])(a => a.asRight[String]).toEitherT[Future]

  def canSpecialMove(ally1:String, ally2:String): ResponseT[Boolean] = for {
    a1 <- getPowerLevel(ally1)
    a2 <- getPowerLevel(ally2)
  } yield a1 + a2 > 15

  def tacticalReport(ally1:String, ally2:String): String = Await.result(canSpecialMove(ally1,ally2).map(a => if (a) "pueden" else "no pueden").merge,Duration.Inf)

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz","Bumblebee"))
    println(tacticalReport("Hot Rod","Bumblebee"))
    println(tacticalReport("Hot Rod","Alf"))
    println(tacticalReport("Alf Rod","Alf"))
  }
}
