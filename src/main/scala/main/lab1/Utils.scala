package main.lab1

import scala.collection.mutable

object Utils {
  /**
    * y1 = f1(x, y1, y2, y3)
    * y2 = f2(x, y1, y2, y3)
    * y3 = f3(x, y1, y2, y3)
    *
    * y1(a) = y1_0
    * y2(a) = y2_0
    * y3(a) = y3_0
    *
    * x \in [a, b]
    *
    * n -- number of intervals
    *
    * @return (y1, y2, y3)
    */
  def rungeKuttaSystem(
                        f1: (Double, Double, Double, Double) => Double,
                        f2: (Double, Double, Double, Double) => Double,
                        f3: (Double, Double, Double, Double) => Double,
                        y1_0: Double,
                        y2_0: Double,
                        y3_0: Double,
                        a: Double,
                        b: Double,
                        n: Int
                      ): (Seq[Double], Seq[Double], Seq[Double]) = {
    val h = (b - a) / n
    val xs = Seq.iterate(a, n)(_ + h)
    var y1s = mutable.ListBuffer(y1_0)
    var y2s = mutable.ListBuffer(y2_0)
    var y3s = mutable.ListBuffer(y3_0)

    var y1 = y1_0
    var y2 = y2_0
    var y3 = y3_0

    for (x <- xs) {
      val k11 = h * f1(x, y1, y2, y3)
      val k12 = h * f2(x, y1, y2, y3)
      val k13 = h * f3(x, y1, y2, y3)

      val k21 = h * f1(x + h / 2, y1 + k11 / 2, y2 + k12 / 2, y3 + k13 / 2)
      val k22 = h * f2(x + h / 2, y1 + k11 / 2, y2 + k12 / 2, y3 + k13 / 2)
      val k23 = h * f3(x + h / 2, y1 + k11 / 2, y2 + k12 / 2, y3 + k13 / 2)

      val k31 = h * f1(x + h / 2, y1 + k21 / 2, y2 + k22 / 2, y3 + k23 / 2)
      val k32 = h * f2(x + h / 2, y1 + k21 / 2, y2 + k22 / 2, y3 + k23 / 2)
      val k33 = h * f3(x + h / 2, y1 + k21 / 2, y2 + k22 / 2, y3 + k23 / 2)

      val k41 = h * f1(x + h, y1 + k31, y2 + k32, y3 + k33)
      val k42 = h * f2(x + h, y1 + k31, y2 + k32, y3 + k33)
      val k43 = h * f3(x + h, y1 + k31, y2 + k32, y3 + k33)

      y1 = y1 + (k11 + 2 * k21 + 2 * k31 + k41) / 6
      y2 = y2 + (k12 + 2 * k22 + 2 * k32 + k42) / 6
      y3 = y3 + (k13 + 2 * k23 + 2 * k33 + k43) / 6

      y1s += y1
      y2s += y2
      y3s += y3
    }

    (y1s, y2s, y3s)
  }
}
