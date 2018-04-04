package main.lab1

import Math._

object Main {
  val header = Seq(Seq("x", "u", "u'", "u''", "Δu", "Δu'", "Δu''"))

//    val filename = "source"
//    def f(x: Double, u: Double, v: Double, w: Double): Double = x
//    def sol(x: Double): Double = 1.0/24*x*(x*x*x + 12*x - 106)
//    def solD(x: Double): Double = x*x*x/6.0 + x - 53.0/12
//    def solDD(x: Double): Double = (1.0/2)*(x*x + 2)

  val filename = "source1"
  def f(x: Double, u: Double, v: Double, w: Double): Double = 24 * x - 12 * x * x * (4 * x * x * x + 5) + w * v
  def sol(x: Double): Double = x * x * x * x + 5 *x - 1
  def solD(x: Double): Double = 4 * x * x * x + 5
  def solDD(x: Double): Double = 12 * x * x

//  val filename = "source2"
//  def f(x: Double, u: Double, v: Double, w: Double): Double = 12 + 4 * x
//  def sol(x: Double): Double = 1.0 / 6 * x * (x * x * x + 12 * x * x + 36 * x + 23)
//  def solD(x: Double): Double = (2 * x * x * x) / 3 + 6 * x * x + 12 * x + 23.0 / 6
//  def solDD(x: Double): Double = 2 * (x * x + 6 * x + 6)

  def main(args: Array[String]): Unit = {
    printReport()
  }

  def printReport(): Unit = {
    val (errorCode, ys, alpha) = new Shooting(filename, f).apply
    errorCode match {
      case 0 =>
        println(s"alpha = $alpha")
        val table = ys
          .map({ case (x, y1, y2, y3) =>
            Seq(x, y1, y2, y3,
              abs(y1 - sol(x)),
              abs(y2 - solD(x)),
              abs(y3 - solDD(x)))
          })
        println(utils.Tabulator.format(header ++ table))

      case 1 => println("iterations limit exceeded")
    }
  }
}

//        Vegas("Name")
//            .withData(table
//              .map({ x =>
//                  Map("x" -> x(0), "y" -> x(1))
//              }))
//          .mark(Line)
//          .encodeX("x", Temp)
//          .encodeY("y", Quant)
//          .show
