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
  * u(a) = A
  * w(a) = B
  * u(a) = alpha
  */
class Shooting(filename: String) {
  val (_A, _B, _C, a, b, n, eps, maxIterations, alpha0) = getParams()

  /**
    * f function from description above
    */
  def f(x: Double, a: Double, b: Double, c: Double): Double = 1

  /**
    * Read parameters from file
    * @return all the parameters
    */
  def getParams(): (Double, Double, Double, Double, Double, Int, Double, Int, Double) = {
    (1, 1, 1, 1, 1, 1, 1, 1, 1)
  }

  /**
    * phi(alpha) = u(b, alpha) - C
    *
    * (we need to find alpha with phi(alpha) = 0)
    */
  def phi(alpha: Double): Double = {
    val u = Utils.rungeKuttaSystem(
      f,
      (_, _, _, w) => w,
      (_, _, v, _) => v,
      _A, _B, alpha,
      a, b, n
    )._3.last

    abs(u - _C)
  }

  /**
    * I don't know what will be here yet
    *
    * but it's probably main method
    */
  def apply: Unit = {

  }
}
