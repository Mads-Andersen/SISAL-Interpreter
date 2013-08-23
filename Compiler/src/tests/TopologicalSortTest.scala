package tests

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import parser._
import interpreter.TopologicalSort

class TopologicalSortTest extends FunSuite {
  test("Graph 1") {
    val e1 = (0,1)
    val e2 = (0,1)
    val e3 = (0,3)
    val e4 = (0,4)
    val e5 = (0,4)
    val e6 = (0,2)
    val e7 = (1,2)
    val e8 = (3,4)
    val e9 = (2,5)
    val e10 = (4,5)
    val e11 = (5,0)
    val edges = List(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11)
    val result = TopologicalSort.topSort(edges)
    println(result)
  }
  
  test("Wiki graph") {
    val e1 = (7,11)
    val e2 = (7,8)
    val e3 = (5,11)
    val e4 = (3,8)
    val e5 = (3,10)
    val e6 = (11,2)
    val e7 = (11,9)
    val e8 = (11,10)
    val e9 = (8,9)
    val edges = List(e1,e2,e3,e4,e5,e6,e7,e8,e9)
    val result = TopologicalSort.topSort(edges)
    println(result)
  }
}