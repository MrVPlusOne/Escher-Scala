package escher

import escher.DSL.{tyList, tyVar}
import escher.Synthesis.ArgList

import scala.collection.mutable
import DSL._

/**
  * Created by weijiayi on 14/05/2017.
  */
object GenExamples {
  def compress[A](buffer: mutable.Map[List[A], List[A]]): List[A] => List[A] = {
    def f(xs: List[A]): List[A] = {
      val result = xs match {
        case a::b::tail => if(a==b) f(b::tail) else a :: f(b::tail)
        case _ => xs
      }
      buffer(xs) = result
      result
    }
    f
  }

  def main(args: Array[String]): Unit = {
    val buffer: mutable.Map[List[Int], List[Int]] = mutable.Map()
    val impl = compress(buffer)
    val inputs = List(
      List(),
      List(7),
      List(3, 9),
      List(9, 9),
      List(2, 3, 9),
      List(9, 9, 2),
      List(3, 3, 3, 9),
      List(2, 3, 3, 9, 9)
    )
    inputs.foreach(in => impl(in))
    buffer.toList.map(_._1.toString).sorted.foreach(println)
  }
}
