package interpreter

import InterpreterSeq._
import Interpreter._
import parser._
import parser.Type._

object UtilityCNode {

  def evalSelect(node: NSelect, values: Seq[Literal]) = {
    val selector = node.graphs(node.associations(0))
    val selectorResult = evalGraph(selector, values).collect { case x: LInteger => x }
    val index = selectorResult(0)
    val selectedGraph = node.graphs(node.associations(index.value + 1))
    evalGraph(selectedGraph, values)
  }

  def evalTagCase(n: NTagCase, values: Seq[Literal]) = {
    ???
  }

  def evalForAll(n: NForAll, values: Seq[Literal]) = {
    val generator = n.graphs(n.associations(0))
    val body = n.graphs(n.associations(1))
    val results = n.graphs(n.associations(2))

    val generatorResult = evalGraph(generator, values)
    val multiples = generatorResult.collect { case x: LMultiple => x }
    val gathered = if(Runner.parallel) forAllParallel(body, multiples, values) else forAllSerial(body, multiples, values)
    val newMultiples = gathered.toList.map(x => LMultiple(x))

    evalGraph(results, values ++ generatorResult ++ newMultiples)
  }
  
  private def forAllSerial2(body: Graph, schedueled: Seq[Literal], values: Seq[Literal]) = {
    val completed = schedueled.map(x => evalGraph(body, values :+ x))
    val gathered = (for (x <- 0 until completed(0).size) yield completed.map(_.apply(x)))
    gathered
  }
  
  private def forAllParallel2(body: Graph, schedueled: Seq[Literal], values: Seq[Literal]) = {
    val completed = schedueled.par.map(x => evalGraph(body, values :+ x))
    val gathered = (for (x <- 0 until completed(0).size) yield completed.seq.map(_.apply(x)))
    gathered
  }
  
  private def forAllSerial(body: Graph, multiples: Seq[LMultiple], values: Seq[Literal]) = {
    val schedueled = (for (x <- 0 until multiples(0).values.size) yield multiples.map(_.values.apply(x)))
    val completed = schedueled.map(x => evalGraph(body, values ++ x))
    val gathered = (for (x <- 0 until completed(0).size) yield completed.map(_.apply(x)))
    gathered
  }
  
  private def forAllParallel(body: Graph, multiples: Seq[LMultiple], values: Seq[Literal]) = {
    val schedueled = (for (x <- 0 until multiples(0).values.size) yield multiples.map(_.values.apply(x)))
    val completed = schedueled.par.map(x => evalGraph(body, values ++ x))
    val gathered = (for (x <- 0 until completed(0).size) yield completed.seq.map(_.apply(x)))
    gathered
  }

  def evalLoopA(n: NLoopA, values: Seq[Literal]) = {
    val init = n.graphs(n.associations(0))
    val test = n.graphs(n.associations(1))
    val body = n.graphs(n.associations(2))
    val returns = n.graphs(n.associations(3))

    val initResult = evalGraph(init, values)
    val bodyResult = whileLoopA(test, body, values, initResult)
    evalGraph(returns, bodyResult)
  }

  def evalLoopB(n: NLoopB, values: Seq[Literal]) = {
    val init = n.graphs(n.associations(0))
    val test = n.graphs(n.associations(1))
    val body = n.graphs(n.associations(2))
    val returns = n.graphs(n.associations(3))

    val initResult = evalGraph(init, values)
    val bodyResult = whileLoopB(test, body, values, initResult)
    evalGraph(returns, bodyResult)
  }
  
  def whileLoopA(test: Graph, body: Graph, imports: Seq[Literal], values: Seq[Literal]): GraphResult = {
    val bodyResult = evalGraph(body, imports ++ values)
    val testResult = evalGraph(test, imports ++ values).collect{case x:LBoolean => x}
    val condition = testResult(0).value 
    if(condition) whileLoopA(test, body, imports, bodyResult) else (imports ++ values)
  }

  def whileLoopB(test: Graph, body: Graph, imports: Seq[Literal], values: Seq[Literal]): GraphResult = {
    val testResult = evalGraph(test, imports ++ values).collect { case x: LBoolean => x }
    val condition = testResult(0).value
    if (condition) {
      val bodyResult = evalGraph(body, imports ++ values)
      whileLoopB(test, body, imports, bodyResult)
    } else {
      imports ++ values
    }
  }
}