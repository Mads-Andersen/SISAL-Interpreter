package interpreter

import parser._
import parser.Type._
import org.scalatest.FunSuite

object TopologicalSort {
  
  def sort(graph: Graph) = {
    val nodes = Graph.getNodes(graph)
    val edges = Graph.getEdges(graph)
    val edgelist = edges.map(x => (x.sNode, x.dNode))
    val result = topSort(edgelist.toList)
    val options = result.map(x => nodes.find(node => (node.id == x)))
    options.flatten
  }
  
  private def removeVertexs(edges: List[(Int, Int)]) : List[Int] = edges match {
    case Nil => List()
    case _ =>
      val src_vertexs = (edges.unzip _1).toSet.diff(List().toSet).toList
      val dst_vertexs = (edges.unzip _2).toSet.diff(List(0).toSet).toList
      val all_vertexs = src_vertexs ++ dst_vertexs
      val vertex_no_incoming = all_vertexs.toSet.diff(dst_vertexs.toSet).toList
      val new_edges = edges.filter((x) => (x._1 != vertex_no_incoming(0)))
      vertex_no_incoming(0) :: (removeVertexs(new_edges))
  }
  
  def topSort(edges: List[(Int,Int)]) : List[Int] = {
    val all_vertexs = (edges.unzip _1) ++ (edges.unzip _2)
    val remove = removeVertexs(edges)
    val isolated = all_vertexs.toSet.diff(remove.toSet).toList
    val boundaries = List(0, Int.MaxValue)
    val result = remove ++ isolated
    result.diff(List(0, Int.MaxValue))
  }
}

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