package org.alfiler

import cats.data.Writer
import cats.{Eval, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.{Await, Future}
import scala.language.higherKinds

object CatsMain {

  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.vector._


  def sum[F[_]:Monad](a:F[Int],b:F[Int]):F[Int] = {
    for {
      x <- a
      y <- b
    } yield x + y
  }


  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    println(Await.result(Future.sequence(Vector(Future(factorial(3)),Future(factorial(3)))), 5.seconds))
  }

  def factorialBig(n:BigInt):Eval[BigInt] = {
    if (n == 1) {
      Eval.now(1)
    } else {
      Eval.defer(factorialBig(n-1).map(_ * n))
    }
  }

  import cats.syntax.writer._

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n:Int):Writer[Vector[(Int, Int)],Int] = {
    slowly(if(n == 0) 1.writer(Vector((n,1))) else {
      val withValue = factorial(n - 1).map(_ * n)
      withValue.mapWritten(v => v :+ (n, withValue.value))
    })
  }
}
