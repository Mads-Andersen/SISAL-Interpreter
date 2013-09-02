package interpreter

import interpreter._
import parser._
import parser.Type._
import utility._
import partitioner._

object Runner {

  var parallel = false
  var program = Program(List(), List())
  val squares = Program(SISAL.compileFile("../Programs/Squares.sis"), List(LInteger(5)))
  
  def main(args: Array[String]) {
    GraphPrinter.openGraphs(partition(Runner.squares).map(x => x.map(p => p.graph)).flatten)
  }
  
  def partition(program: Program) = {  
    Runner.program = program
    val graphs = CostAssignment.assign(program)
    val partitions = GraphExpansion.partition(graphs)   
    partitions
  }  
  
}