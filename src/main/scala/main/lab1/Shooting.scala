package main.lab1

import Math.abs

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
  * u' = v              u(a) = A
  * v' = w              v(a) = alpha
  * w' = f(x, u, v, w)  w(a) = B
  * u(a) = A
  * v(a) = alpha
  * w(a) = B
  */
class Shooting(filename: String) {
  val (_A, _B, _C, a, b, n, eps, maxIterations, alpha0) = getParams(filename)

  /**
    * f function from description above
    */
  def f(x: Double, u: Double, v: Double, w: Double): Double = 1

  def sol(x: Double): Double = x * x * x / 6 + x * x + x

  /**
    * Read parameters from file
    * @return all the parameters
    */
  def getParams(filename: String): (Double, Double, Double, Double, Double, Int, Double, Int, Double) = {
    (0, 2, sol(1), 0, 1, 10, 0.001, 10000, 1)
  }

  /**
    * phi(alpha) = u(b, alpha) - C
    * (we need to find alpha with phi(alpha) = 0)
    *
    * (just a note)
    * u' = v              u(a) = A
    * v' = w              v(a) = alpha
    * w' = f(x, u, v, w)  w(a) = B
    */
  def phi(alpha: Double): Double = {
    val u = Utils.rungeKuttaSystem(
      (_, _, v, _) => v,
      (_, _, _, w) => w,
      f,
      _A, alpha, _B,
      a, b, n
    )

    abs(u._1.last - _C)
  }

  /**
    * I don't know what will be here yet
    *
    * but it's probably main method
    */
  def apply: Unit = {
    println(s"phi = ${phi(alpha0)}")
    println()

    val us = Utils.rungeKuttaSystem(
      (_, _, v, _) => v,
      (_, _, _, w) => w,
      f,
      _A, alpha0, _B,
      a, b, n
    )._1

    val h = (b - a) / n
    val xs = (0 to n).map(a + _ * h)

    val ys = xs.map(sol)

    assert(us.lengthCompare(ys.length) == 0)

    println("etc")
    for ((u, y) <- us.zip(ys)) {
      println(s"${u - y}")
    }
  }
}
