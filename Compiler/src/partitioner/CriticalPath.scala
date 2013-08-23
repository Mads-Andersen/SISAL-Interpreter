package partitioner

import parser._
import parser.Type._
import utility.GraphPrinter

object CriticalPath {

  type Destinations = Map[SuperNode, Double]
  type Predecessors = Map[SuperNode, Option[SuperNode]]
  
  def value(graphs: CostGraphs) : Double = {
    val supergraph = GraphDependence.convert(graphs)
    val sources = GraphDependence.findSources(supergraph._1, supergraph._2)
    sources.map(x => CriticalPath.value(supergraph._1, supergraph._2, x)).max
  }
  
  private def value(nodes: SuperNodes, edges: SuperEdges, source: SuperNode) = {
    var (destination, predecessor) = init(nodes, source)
    
    for(i <- nodes) {
      for(n <- nodes) {
        for(v <- GraphDependence.findChildren(n, edges)) {
          var (d,p) = relax(n, v, destination, predecessor)
          destination = d
          predecessor = p
        }
      }
    }
    -(destination.values.min)
  }
  
  private def init(nodes: SuperNodes, source: SuperNode) = {
    var destination = Map[SuperNode, Double]()
    var predecessor = Map[SuperNode, Option[SuperNode]]()
    
    for(n <- nodes) {
      destination += (n -> Int.MaxValue)
      predecessor += (n -> None)
    }
    destination += (source -> -TimeModel.time(source))
    (destination, predecessor)
  }
  
  private def relax(node: SuperNode, neighbor: SuperNode, dests: Destinations, preds: Predecessors) = {
    var d = dests
    var p = preds
    
    val edgecost = -TimeModel.time(neighbor)
    if(d(neighbor) > d(node) + edgecost) {
      d += (neighbor -> (d(node) + edgecost))
      p += (neighbor -> Some(node))
      (d, p)
    }
    else {
      (d, p)
    }
  }  
}