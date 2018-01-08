package learning.scala

trait Chapter5 {

  val max = (a: Int, b: Int) => if (a > b) a else b

  def max3(t: (Int, Int, Int)) (f:(Int, Int) => Int) =
    f(f(t._1, t._2), t._3)

}
