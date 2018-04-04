package main.lab1

import Math.abs

import main.lab1.utils.Utils

import scala.io.Source

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
class Shooting(filename: String, f: (Double, Double, Double, Double) => Double) {
  val (_A, _B, _C, a, b, n, eps, maxIterations, alpha0, dAlpha) = getParams(filename)

  /**
    * f function from description above
    */
  //  def f(x: Double, u: Double, v: Double, w: Double): Double = 1

  def isSolution(phi: Double): Boolean = abs(phi) <= eps

  /**
    * Read parameters from file
    *
    * @return all the parameters
    */
  def getParams(filename: String): (Double, Double, Double, Double, Double, Int, Double, Int, Double, Double) = {
    val map = Source.fromFile(filename).getLines.filter(_.contains("=")).map(_.trim.split("=")).map(x => (x(0), x(1))).toMap
    (
      map.getOrElse("A", "").toDouble,
      map.getOrElse("B", "").toDouble,
      map.getOrElse("C", "").toDouble,
      map.getOrElse("a", "").toDouble,
      map.getOrElse("b", "").toDouble,
      map.getOrElse("n", "").toInt,
      map.getOrElse("eps", "").toDouble,
      map.getOrElse("mi", "").toInt,
      map.getOrElse("a0", "").toDouble,
      map.getOrElse("da", "").toDouble
    )
  }

  def rk(alpha: Double): Seq[(Double, Double, Double, Double)] = {
    Utils.rungeKuttaSystem(
      (_, _, v, _) => v,
      (_, _, _, w) => w,
      f,
      _A, alpha, _B,
      a, b, n
    )
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
    rk(alpha).last._2 - _C
  }

  /**
    * Find a fork [alpha0, alpha1] (and its phi's)
    */
  def alphas(a0: Double, dAlpha: Double): (Double, Double, Double, Double) = {
    var alpha0 = a0
    var phi0 = phi(alpha0)
    var phi1 = phi(alpha0 + dAlpha)
    if (phi0 * phi1 < 0 || isSolution(phi0) || isSolution(phi1)) {
      (alpha0, alpha0 + dAlpha, phi0, phi1)
    } else if ((phi0 - phi1) * phi0 < 0) {
      alphas(alpha0, -dAlpha)
    } else {
      while (phi1 * phi0 > 0 && !isSolution(phi1)) {
        alpha0 += dAlpha
        phi0 = phi1
        phi1 = phi(alpha0 + dAlpha)
      }

      (alpha0, alpha0 + dAlpha, phi0, phi1)
    }
  }

  def calcAlpha(): Double = {
    var (a0, a1, phi0, phi1) = alphas(alpha0, dAlpha)
    if (isSolution(phi0)) {
      println("123")
      a0
    } else if (isSolution(phi1)) {
      println("321")
      a1
    } else {
      for (i <- 0 to maxIterations) {
        if (abs(a0 - a1) <= 2 * eps) {
          println(i)
          return (a0 + a1) / 2.0
        }

        val ac = (a0 + a1) / 2.0
        val phiC = phi(ac)
        if (isSolution(phiC)) {
          println(i)
          return ac
        }
        if (phi0 * phiC < 0) {
          phi1 = phiC
          a1 = ac
        } else {
          phi0 = phiC
          a0 = ac
        }
      }
      throw new Exception()
    }
  }

  def apply: (Int, Seq[(Double, Double, Double, Double)], Double) = {
    try {
      val alpha = calcAlpha()
      (0, rk(alpha), alpha)
    } catch {
      case _: Exception =>
        (1, Seq(), Double.NaN)
    }

  }
}
