package interpreter

import parser._
import interpreter._
import sys.process._
import scala.annotation.tailrec
import scala.collection.mutable.MutableList

object Utility {
  
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    println(result)
    result
  }
  
  def time[R](code: => R, iterations: Int): R = {
    var i = 0
    var times = MutableList[Long]()
    while(i < iterations) {
      val t0 = System.currentTimeMillis()
      val result = code
      val t1 = System.currentTimeMillis()
      times += (t1-t0)
      i = i+1
    }
    val results = times.sorted
    val average = results.sum/iterations
    val median = results(iterations/2)
    val best = results(0)
    println("Average time to compute a block: " + average + "ms")
    println("Median time to compute a block: " + median + "ms")
    println("Best time to compute a block: " + best + "ms")
    code
  }
}