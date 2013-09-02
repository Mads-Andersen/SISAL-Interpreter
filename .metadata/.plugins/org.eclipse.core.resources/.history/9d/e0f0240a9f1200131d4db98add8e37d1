package partitioner

import parser._
import parser.Type._
import interpreter.TopologicalSort

object SCC {
  
  def execute(program: Program) : Graphs = {
    Partitioner.program = program
    val option = program.graphs.find(x => x.name.value.toUpperCase() == "MAIN")
    val main = option.getOrElse(program.graphs(0))
    execute(main)
  }
  
  private def execute(graph: Graph) : Graphs = {
    val functions = order(graph, List(graph))
    functions.reverse
  }
  
  private def order(graph: Graph, history: Graphs) : Graphs = {
    val nodes = Graph.getNodes(graph)
    val graphs = order(graph, nodes, history)
    graphs
  }
  
  private def order(graph: Graph, nodes: Nodes, history: Graphs) : Graphs = nodes match {
    case Nil => history
    case h :: t => order(graph, t, order(graph, h, history))
  }

  private def order(graph: Graph, node: Node, history: Graphs) : Graphs = node match {
    case n: NCall =>
      val edges = Graph.getEdges(graph)
      val nameLiteral = edges.find(x => x.dNode == node.id && x.dPort == 1)
      nameLiteral match {
        case None => history
        case Some(x) => x match {
          case e: PREdge => history
          case e: PLEdge =>
            val g = Partitioner.program.graphs.find(x => x.name == e.value).get
            history.contains(g) match {
              case false => order(g, history :+ g)
              case true => history
            }
        }
      }
    case n: PCNode => 
      val newCalls = n.graphs.map(x => order(x, history).filter(y => !history.contains(y))).flatten
      history ++ newCalls
    case _ => history
  }
}