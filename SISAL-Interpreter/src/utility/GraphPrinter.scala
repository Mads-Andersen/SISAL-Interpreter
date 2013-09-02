package utility

import scala.annotation._
import scala.util.Random
import sys.process._
import java.io._
import scala.sys.process.Process
import parser.Type._
import parser._
import interpreter._

object GraphPrinter {
  
  var curGraph = PLGraph(LString(""), 0, List(), PMetaInfo(List()))
  
  def main(args: Array[String]) {
    openGraph(Runner.squares.graphs(0))
  }
  
  def openGraph(graph: Graph) {
    val path = "../Graphs/Example.DOT"
    val data = printGraph(graph)
    val writer = new PrintWriter(new File(path))
    writer.write(data)
    writer.close()
    Process("/usr/local/bin/dot", Seq("-T", "pdf", "-O", "../Graphs/Example.DOT"))! ;
    Process("open", Seq("../Graphs/Example.DOT.pdf"))! ;
  }
  
  def openGraphs(graphs: Graphs) {
    val path = "../Graphs/Example.DOT"
    val data = printGraphs(graphs)
    val writer = new PrintWriter(new File(path))
    writer.write(data)
    writer.close()
    Process("/usr/local/bin/dot", Seq("-T", "pdf", "-O", "../Graphs/Example.DOT"))! ;
    Process("open", Seq("../Graphs/Example.DOT.pdf"))! ;
  }
  
  def printGraph(graph: Graph) = {
    val start = s"""digraph G {\n"""
    val attributes = printGraphAttributes()
    val g = printSubgraph(1, graph)
    val edges = printEdges(graph)
    val end = "}"
    start + attributes + g + edges + end
  }
  
  def printGraphs(graphs: Graphs) = {
    val start = s"""digraph G {\n"""
    val attributes = printGraphAttributes()
    val g = printSubgraphs(1, graphs)
    val edges = printEdges(graphs)
    val end = "}"
    start + attributes + g + edges + end
  }
  
  private def printSubgraphs(level: Int, graphs: Graphs): String = printSubgraphs(level, graphs, 0, "")
  private def printSubgraphs(level: Int, graphs: Graphs, id:Int, value: String) : String = {
    graphs match {
      case Nil => value
      case h::t => printSubgraphs(level, t, id+1, value + printSubgraph(level, id, h))
    }
  }
  
  private def printSubgraph(level: Int, graph:Graph): String = printSubgraph(level, 0, graph)
  private def printSubgraph(level: Int, id:Int, graph: Graph) = {
    curGraph = graph
    val indentation = indent(level)
    val identifier = graph.identifier
    val start = s"""$indentation subgraph clusterGraph$identifier { \n """
    val label = s"""$indentation label="Graph $id" \n"""
    val end = s"""$indentation }\n"""
     
    val nodes = printNodes(level, Graph.getNodes(graph))
    val literals = printLiterals(level, Graph.getEdges(graph))
    val inputPorts = printInputPorts(level, graph)
    val outputPorts = printOutputPorts(level, graph)
    start + label + nodes + literals + inputPorts + outputPorts + end
  }
  
  private def printNodes(level: Int, nodes: Nodes) : String = nodes.map(x => printNode(level, x)).mkString  
  private def printNode(level: Int, node: Node) : String = node match {
    case n:PSNode => printSNode(level, n)
    case n:PCNode => printCNode(level, n)
  }
  
  private def printSNode(level: Int, node: PSNode) = {
    val indentation = indent(level)
    val identifier = node.identifier
    val id = node.id
    val name = node.getClass().getSimpleName()
    val ports = printPorts(node)
    s"""$indentation $identifier [label="$id: $name|$ports"];\n"""
  }
  
  private def printCNode(level: Int, node: PCNode) : String = {
    val indentation1 = indent(level)
    val indentation2 = indent(level+1)
    val identifier = node.identifier
    val name = node.getClass().getSimpleName()
    val id = node.id
    val ports = printPorts(node)
    val associations = node.associations
    val graphs = printSubgraphs(level+1, node.graphs)
    val start = s"""$indentation1 subgraph clusterCNode$identifier { \n"""
    val label = s"""$indentation2 label="$associations"; \n"""
    val value = s"""$indentation2 $identifier [label="$id: $name|$ports"]; \n"""
    val end = s"""$indentation1 }\n """
    s"""$start $label $value $graphs $end"""
  }
  
  private def printInputPorts(level: Int, graph: Graph) : String = {
    val indentation = indent(level)
    val identifier = getGraphInputBoundaryIdentifier(graph)
    val ports = Graph.getInputPorts(graph).toSet.toList
    if(ports.size == 0) return ""
    val label = printPorts(ports, "")
    s"""$indentation $identifier [label="$label", fillcolor="#00A388"]\n"""
  }
  
  private def printOutputPorts(level: Int, graph: Graph) : String = {
    val indentation = indent(level)
    val identifier = getGraphOutputBoundaryIdentifier(graph)
    val ports = Graph.getOutputPorts(graph).toSet.toList
    if(ports.size == 0) return ""
    val label = printPorts(ports, "")
    s"""$indentation $identifier [label="$label", fillcolor="#FF6138"]\n"""
  }
  
  private def printPorts(node: Node) : String = {
    val input = Node.getInputPorts(node, curGraph).toSet
    val output = Node.getOutputPorts(node, curGraph).toSet
    val ports = (input ++ output).toList.sortBy(x=>x)
    printPorts(ports, "")
  }
  
  private def printPorts(ports: Seq[Int], value: String) : String = ports match {
    case Nil => value
    case h::Nil => printPorts(Nil, s"""$value<$h>$h""")
    case h::t => printPorts(t, s"""$value<$h>$h |""")
  }
  
  private def printLiterals(level: Int, edges: Edges) : String = edges.map(x => printLiteral(level, x)).mkString  
  private def printLiteral(level: Int, edge: Edge) : String = edge match {
    case x: PLEdge => printLiteral(level, x)
    case _ => ""
  }
  
  private def printLiteral(level: Int, edge: PLEdge) : String = {
    val indentation = indent(level)
    val identifier = edge.value.identifier
    val name = edge.value.toString()
    s"""$indentation $identifier [label="$name", fillcolor="#FFB03B", style="filled"];\n"""
  }
  
  private def printEdges(graphs: Graphs) : String = graphs.map(x => printEdges(x)).mkString
  private def printEdges(graph: Graph) : String = {
    val compoundEdges = printCompoundEdges(Graph.getNodes(graph))
    val edges = Graph.getEdges(graph).foldLeft(""){(edges, edge) => edges + printEdge(edge, graph)}
    edges + compoundEdges
  }
  
  private def printCompoundEdges(nodes: Nodes) : String = nodes.map(x => printCompoundEdges(x)).mkString
  private def printCompoundEdges(node: Node) = node match{
    case n: PCNode => printEdges(n.graphs)
    case _ => ""
  }
  
  private def printEdge(edge: Edge, graph: Graph) = edge match {
    case e: PREdge => printREdge(e, graph)
    case e: PLEdge => printLEdge(e, graph)
  }
  
  private def printREdge(edge: PREdge, graph: Graph) = {
    val indentation = indent(1)
    val source = Edge.getSourceNode(edge, graph)
    val destination = Edge.getDestinationNode(edge, graph)
    val sourceId = getSourceNodeIdentifier(graph, edge)
    val destinationId = getDestinationNodeIdentifier(graph, edge)
    s"""$indentation $sourceId->$destinationId;\n"""
  }
  
  private def printLEdge(edge: PLEdge, graph: Graph) = {
    val indentation = indent(1)
    val destination = Edge.getDestinationNode(edge, graph)
    val sourceId = getSourceNodeIdentifier(graph, edge)
    val destinationId = getDestinationNodeIdentifier(graph, edge)
    s"""$indentation $sourceId->$destinationId;\n"""
  }
  
  private def getSourceNodeIdentifier(graph: Graph, edge: Edge) = edge match {
    case x:PLEdge => x.value.identifier
    case x:PREdge => Edge.getSourceNode(edge, graph) match {
      case None => getGraphInputBoundaryIdentifier(graph) + ":" + edge.sPort
      case Some(n) => n.identifier + ":" + edge.sPort
    }
  }
  
  private def getDestinationNodeIdentifier(graph: Graph, edge: Edge) = {
    Edge.getDestinationNode(edge, graph) match {
      case None => getGraphOutputBoundaryIdentifier(graph) + ":" + edge.dPort
      case Some(n) => n.identifier + ":" + edge.dPort
    }
  }
  
  private def getGraphInputBoundaryIdentifier(graph: Graph) = {
    graph.identifier + 1000
  }
  
  private def getGraphOutputBoundaryIdentifier(graph: Graph) = {
    graph.identifier + 2000
  }
  
  private def printGraphAttributes() = {
    val indentation = indent(1)
    val compound = s"""$indentation compound=true;\n"""
    val nodes = s"""$indentation node [fillcolor="#7690CF", style="rounded,filled",shape=record, height=0.2, width=0.2];\n"""
    val size = s"""$indentation ranksep=.75; size = "7.5,7.5"; \n"""
    s"""$compound $nodes $size"""
  }
  
  private def indent(level: Int): String  = {
    indent(level, "")
  }
  
  private def indent(level:Int, value:String): String = level match{
    case 0 => value
    case _ => indent(level-1, value + "\t")
  } 
}