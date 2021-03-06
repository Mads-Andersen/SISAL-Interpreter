package partitioner

import parser._
import parser.Type._
import interpreter._
import interpreter.TopologicalSort
import utility.GraphPrinter

object GraphMerger {
  
  def createGraph(graph: CostGraph, node: CostNode) = {
    val inputs = NodeCost.getInputEdges(node, graph)
    val outputs = NodeCost.getOutputEdges(node, graph)   
    val edges = inputs ++ outputs   
    val subgraph = PLGraph(LString(""), 0, node.node +: edges, PMetaInfo(List()))
    val costgraph = CostGraph(subgraph, List(node), edges)
    costgraph
  }
  
  def merge(supergraph: CostGraph, subgraph1: Subgraph, subgraph2: Subgraph, subgraphs: Subgraphs) = {
    val nodeIDs = getNodeIDs(supergraph, subgraph1, subgraph2)
    val subgraphsToMerge = subgraphs.filter(s => s.nodes.exists(x => nodeIDs.contains(x.node.id)))
    val subgraphsNotToMerge = subgraphs.diff(subgraphsToMerge)
    val cnodes = subgraphsToMerge.map(x => x.nodes).flatten.toSet.toList
    val cedges = subgraphsToMerge.map(x => x.edges).flatten.toSet.toList
    val pnodes = cnodes.map(x => x.node)
    val pedges = cedges
    val subgraph = PLGraph(LString(""), 0, pnodes ++ pedges, PMetaInfo(List()))
    val costgraph = CostGraph(subgraph, cnodes, cedges)
    
    val newSubgraphs = subgraphsNotToMerge :+ costgraph
    newSubgraphs
  }
  
  private def getNodeIDs(supergraph: CostGraph, subgraph1: Subgraph, subgraph2: Subgraph) = {
    val edges = supergraph.edges
    val nodes1 = subgraph1.nodes.map(x => x.node.id)
    val nodes2 = subgraph2.nodes.map(x => x.node.id)
    val x = bfsBackward(nodes1, edges, nodes1).intersect(bfsForward(nodes2, edges, nodes2))
    val y = bfsBackward(nodes2, edges, nodes2).intersect(bfsForward(nodes1, edges, nodes1))
    val nodes = (x ++ y).toSet.toList.sorted
    nodes.size match {
      case 0 => (nodes1 ++ nodes2).toSet.toList.sorted// supergraph.nodes.map(x => x.node.id).sorted
      case _ => nodes
    }
  }
  
  private def bfsForward(nodeIDs: IDs, edges: Edges, values: IDs) : IDs = nodeIDs match {
    case Nil => values
    case h :: t =>
      val newIDs = edges.filter(e => h == e.sNode).map(x => x.dNode)
      val children = newIDs.filterNot(x => x == 0 || x == Constant.LiteralSourceNode)
      bfsForward(t ++ children, edges, values ++ children)
  }
  
  private def bfsBackward(nodeIDs: IDs, edges: Edges, values: IDs) : IDs = nodeIDs match {
    case Nil => values
    case h :: t =>
      val newIDs = edges.filter(e => h == e.dNode).map(x => x.sNode)
      val parents =  newIDs.filterNot(x => x == 0 || x == Constant.LiteralSourceNode)
      bfsBackward(t ++ parents, edges, values ++ parents)
  }
}