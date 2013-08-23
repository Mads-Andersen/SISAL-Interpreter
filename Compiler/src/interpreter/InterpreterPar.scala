package interpreter

import parser._
import parser.Type._
import partitioner.GraphExpansion
import partitioner.CostAssignment
import partitioner.GraphDependence

object InterpreterPar {

  var ready:Macros = List()
  var completed:Macros = List()
  
  def main(args: Array[String]) {
  
  }
  
  def eval(program: Program) : GraphResult = {
    
    //Partitioner.program = program    
    val graphs = CostAssignment.assign(program)
    val partitions = GraphExpansion.partition(graphs)
    val mapped = graphs.zip(partitions)
    mapped.last
    //val subgraphs = GraphExpansion.part(main)
    //Interpreter.eval(subgraphs(0), program.arguments)
    //val tasks = Algorithm.partition(main)
    //Interpreter.eval(tasks(0), program.arguments)
    ???
  }
  
  def test(program: Program) : GraphResult = {
    
    //Partitioner.program = program    
    val graphs = CostAssignment.assign(program)
    val partitions = GraphExpansion.partition(graphs)
    val mapped = graphs.zip(partitions)
    val x = mapped.last
   //x._2.map(x => x.)
    //val subgraphs = GraphExpansion.part(main)
    //Interpreter.eval(subgraphs(0), program.arguments)
    //val tasks = Algorithm.partition(main)
    //Interpreter.eval(tasks(0), program.arguments)
    ???
  }
  
  def run(graphs: CostGraphs) = {
    val (nodes, edges) = GraphDependence.convert(graphs)
    val sources = GraphDependence.findSources(nodes, edges)
  }
}