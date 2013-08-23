package interpreter

import utility._
import parser._
import parser.Type._

object InterpreterSeq {
  
  var program = Program(List(), List())
  
  def main(args: Array[String]) {
    InterpreterSeq.program = Runner.test
    val option = program.graphs.find(x => x.name.value.toUpperCase() == "MAIN")
    val main = option.getOrElse(program.graphs(0))
    
    val x = Interpreter.evalGraph(main, List())
    Utility.time(Interpreter.evalGraph(main, List()), 5)
    println(x)
    //Utility.time(eval(Runner.parABCD), 10)
  }
  
  /*
  def eval(program: Program) : GraphResult = {
    InterpreterSeq.program = program
    val option = program.graphs.find(x => x.name.value.toUpperCase() == "MAIN")
    val main = option.getOrElse(program.graphs(0))
    eval(main, program.arguments)
  }
  
  def eval(graph: Graph, arguments: Arguments) : GraphResult = {
    def setup(index: Int, values: Seq[Literal], initEnv: Environment) : Environment = values match {
      case Nil => initEnv
      case h :: t => 
        val argument = h
        val inputs = Graph.getInputEdges(graph, index)
        setup(index+1, t, initEnv ++ inputs.map(x => (x, argument)))
    }
    val setupEnv = setup(1, arguments, List())
    evaluate(graph, setupEnv)
  }
  * 
  */
  
  
}