package org.alfiler

import shapeless.ops.hlist.IsHCons
import shapeless.{HList, HNil, Poly1, Poly2}

case class Yepe(a:String, b:Int)

object size extends Poly1 {
  implicit def caseInt    = at[Int](x ⇒ 1)
  implicit def caseString = at[String](_.length)
  implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) =
    at[(T, U)](t ⇒ size(t._1) + size(t._2))
}

object addSize extends Poly2 {
  implicit def default[T](implicit st: size.Case.Aux[T, Int]) =
    at[Int, T] { (acc, t) => acc + size(t) }
}

object ShaplessTest {
  def main(args: Array[String]): Unit = {
    val l = 23 :: "foo" :: (13, "wibble") :: HNil

    println(l.foldLeft(0)(addSize))
  }


  def algo[L <: HList](hList: L)(implicit c : IsHCons[L]):c.H = c.head(hList)
}
