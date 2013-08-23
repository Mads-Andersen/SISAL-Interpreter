package interpreter

import parser._
import parser.Type._

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