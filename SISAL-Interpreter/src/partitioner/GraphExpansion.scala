package partitioner

import parser.Type._
import parser._
import interpreter.TopologicalSort
import interpreter._
import utility.GraphPrinter

object GraphExpansion { 
  
  def partition(graphs: CostGraphs) : Partitions = {
    graphs.map(x => partition(x))
  }
  
  def partition(graph : CostGraph) : Partition = {
    val macros = graph.nodes.map(x => matchNode(graph, x))
    val history = merging(graph, macros, List(macros))
    val values = history.map(x => TimeModel.fTT(x))
    val index = values.zipWithIndex.minBy(x => x._1)._2
    history(index)
  }
  
  private def merging(overview: CostGraph, subgraphs: Subgraphs, history: Partitions) : Partitions = subgraphs match {
    case h :: Nil => history
    case h :: t => mergingLocal(overview, subgraphs, history)
  }
  
  private def mergingLocal(overview: CostGraph, subgraphs: Subgraphs, history: Partitions) : Partitions = {
    val best = pick(subgraphs)  
    val lowValue = examine(overview, best, subgraphs)
    val partition = GraphMerger.merge(overview, best, lowValue, subgraphs)
    merging(overview, partition, history :+ partition)
  }
  
  private def matchNode(graph: CostGraph, node: CostNode) : Subgraph = node.node match {
    case x: NCall => simpleToTask(graph, node)
    case x: PSNode => simpleToTask(graph, node)
    case x: PCNode => compoundToTask(graph, node)
  }
  
  private def callToTask(graph: CostGraph, node: CostNode) : Subgraph = {
    GraphMerger.createGraph(graph, node)
  }
  
  private def simpleToTask(graph: CostGraph, node: CostNode) : Subgraph = {
    GraphMerger.createGraph(graph, node)
  }
  
  private def compoundToTask(graph: CostGraph, node: CostNode) : Subgraph = {
    //node.graphs.map(x => part(graph)).flatten
    GraphMerger.createGraph(graph, node)
  }
  
  private def examine(overview: CostGraph, selected: CostGraph, tasks: CostGraphs) : Subgraph = {
    val others = tasks.filter(x => x != selected)
    val candidates = others.map(x => TimeModel.fTT(GraphMerger.merge(overview, selected, x, tasks)))
    val minIndex = candidates.zipWithIndex.minBy(x => x._1)._2
    val best = others(minIndex)
    best
  }
  
  private def pick(tasks: CostGraphs) : Subgraph = {
    val values = tasks.map(x => TimeModel.overhead(x))
    val maxIndex = values.zipWithIndex.maxBy(x => x._1)._2
    val best = tasks(maxIndex) 
    best
  }
}