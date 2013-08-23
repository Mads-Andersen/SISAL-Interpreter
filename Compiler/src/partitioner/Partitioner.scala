package partitioner

import parser.Type._
import parser._
import interpreter.TopologicalSort
import interpreter._
import utility.GraphPrinter

object Partitioner {

  var program = Program(List(), List())

  def main(args: Array[String]) {
    //partition(Runner.seqABCD)
    GraphPrinter.openGraphs(partition(Runner.squares).map(x => x.map(p => p.graph)).flatten)
  }

  def partition(program: Program) = {
    Partitioner.program = program    
    val graphs = CostAssignment.assign(program)
    val partitions = GraphExpansion.partition(graphs)   
    partitions
  }  
}