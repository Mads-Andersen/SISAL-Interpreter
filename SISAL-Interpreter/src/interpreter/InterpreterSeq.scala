package interpreter

import utility._
import parser._
import parser.Type._
import partitioner._

object InterpreterSeq {
  
  
  def main(args: Array[String]) {
    val option = Runner.program.graphs.find(x => x.name.value.toUpperCase() == "MAIN")
    val main = option.getOrElse(Runner.program.graphs(0))
    
    println(TimeModel.processors)
    Runner.parallel = true
    Utility.time(Interpreter.evalGraph(main, List(LInteger(5000))), 1000)
    Runner.parallel = false
    Utility.time(Interpreter.evalGraph(main, List(LInteger(5000))), 1000)
  }  
}