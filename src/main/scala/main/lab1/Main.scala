package main.lab1

object Main {
  def main(args: Array[String]): Unit = {
    println(Seq.iterate(0, 5) (_ + 1))
  }
}
