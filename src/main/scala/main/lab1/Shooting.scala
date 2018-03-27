package main.lab1

import Math.abs;

/**
  * u\'\'\' = f(x, u, u', u\'\')
  * u(a) = A
  * u\'\'(a) = B
  * u(b) = C
  *
  * x \in [a, b]
  *
  * aka
  *
  * w' = f(x, u, v, w)
  * v' = w
  * u' = v
  * w(a) = B
  * v(a) = alpha
  * u(a) = A
  */
class Shooting(filename: String) {
  val (_A, _B, _C, a, b, n, eps, maxIterations, alpha0) = getParams(filename)

  /**
    * f function from description above
    */
  def f(x: Double, u: Double, v: Double, w: Double): Double = - w - 1

  /**
    * Read parameters from file
    * @return all the parameters
    */
  def getParams(filename: String): (Double, Double, Double, Double, Double, Int, Double, Int, Double) = {
    // TODO
    (10, 2, -1 / 2 + 6 + 3 * Math.exp(-1) + 7, 0, 1, 100, 0.001, 10000, 3)
  }

  /**
    * phi(alpha) = u(b, alpha) - C
    *
    * (we need to find alpha with phi(alpha) = 0)
    */
  def phi(alpha: Double): Double = {
    // TODO
    val u = Utils.rungeKuttaSystem(
      f,
      (_, _, _, w) => w,
      (_, _, v, _) => v,
      _B, alpha, _A,
      a, b, n
    )

    abs(u._3.last - _C)
  }

  /**
    * I don't know what will be here yet
    *
    * but it's probably main method
    */
  def apply: Unit = {
    val us = Utils.rungeKuttaSystem(
      f,
      (_, _, _, w) => w,
      (_, _, v, _) => v,
      _B, alpha0, _A,
      a, b, n
    )._3

    val h = (b - a) / n
    val xs = Seq.iterate(a, n + 1) (_ + h)

    val ys = xs.map(x => - x * x / 2 + 6 * x + 3 * Math.exp(-x) + 7)

    println(us.length - ys.length)
    println()

    for ((u, y) <- us.zip(ys)) {
      println((u - y))
    }
  }
}
