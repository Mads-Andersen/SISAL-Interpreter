package interpreter

import utility._
import parser._
import parser.Type._

object Interpreter {
  
  def evalGraph(graph: Graph, arguments: Arguments) : GraphResult = {
    val edges = Graph.getInputEdges(graph)
    val env = edges.map(x => (x, arguments(x.sPort-1)))
    evalSubgraph(graph, env)
  }
  
  def evalSubgraph(graph: Graph, env: Environment) : GraphResult = {
    val nodes = Graph.getNodes(graph)
    val newEnv = evaluate(nodes, graph, env)
    newEnv.map(x => Edge.lookup(x._1, newEnv))
  }
  
  private def evaluate(nodes: Nodes, graph: Graph, env: Environment) : Environment = nodes match {
    case Nil => env
    case h :: t => 
      val values = eval(h, graph, env)
      val newEnv = produce(values, h, graph, env)
      evaluate(t, graph, newEnv)
  }

  private def produce(values: Values, node: Node, graph: Graph, env: Environment) = {
    val input = Node.getInputEdges(node, graph)
    val output = Node.getOutputEdges(node, graph)
    val newValues = output.zip(values)
    val tempEnv = env.filter(x => !input.contains(x._1))
    tempEnv ++ newValues
  }

  private def eval(node: Node, graph: Graph, env: Environment): GraphResult = {
    implicit val e = env
    implicit val g = graph
    val values = Node.getInputs(node, graph, env)

    node match {
      case NIPlus(v1: LDouble, v2: LDouble) => List(LDouble(v1.value + v2.value))
      case NIPlus(v1: LInteger, v2: LInteger) => List(LInteger(v1.value + v2.value))
      case NIPlus(v1: LBoolean, v2: LBoolean) => List(LBoolean(v1.value | v2.value))
      case NITimes(p1: LDouble, p2: LDouble) => List(LDouble(p1.value * p2.value))
      case NITimes(p1: LInteger, p2: LInteger) => List(LInteger(p1.value * p2.value))
      case NITimes(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value & p2.value))
      case NIMin(p1: LDouble, p2: LDouble) => List(LDouble(Math.min(p1.value, p2.value)))
      case NIMin(p1: LInteger, p2: LInteger) => List(LInteger(Math.min(p1.value, p2.value)))
      case NIMin(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value & p2.value))
      case NIMax(p1: LDouble, p2: LDouble) => List(LDouble(Math.max(p1.value, p2.value)))
      case NIMax(p1: LInteger, p2: LInteger) => List(LInteger(Math.max(p1.value, p2.value)))
      case NIMax(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value | p2.value))
      case NIMinus(v1: LDouble, v2: LDouble) => List(LDouble(v1.value - v2.value))
      case NIMinus(v1: LInteger, v2: LInteger) => List(LInteger(v1.value - v2.value))
      case NIDiv(p1: LDouble, p2: LDouble) => List(LDouble(p1.value / p2.value))
      case NIDiv(p1: LInteger, p2: LInteger) => List(LInteger(p1.value / p2.value))
      case NIMod(p1: LDouble, p2: LDouble) => List(LDouble(p1.value % p2.value))
      case NIMod(p1: LInteger, p2: LInteger) => List(LInteger(p1.value % p2.value))
      case NIExp(p1: LDouble, p2: LDouble) => List(LDouble(Math.pow(p1.value, p2.value)))
      case NIExp(p1: LInteger, p2: LInteger) => List(LInteger(Math.pow(p1.value, p2.value).toInt))
      case NIAbs(p1: LDouble) => List(LDouble(Math.abs(p1.value)))
      case NIAbs(p1: LInteger) => List(LInteger(Math.abs(p1.value)))
      case NINeg(p1: LDouble) => List(LDouble(-p1.value))
      case NINeg(p1: LInteger) => List(LInteger(-p1.value))
      case NINot(p1: LBoolean) => List(LBoolean(!p1.value))
      case NILess(p1: LDouble, p2: LDouble) => List(LBoolean(p1.value < p2.value))
      case NILess(p1: LInteger, p2: LInteger) => List(LBoolean(p1.value < p2.value))
      case NILess(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value < p2.value))
      case NILess(p1: LCharacter, p2: LCharacter) => List(LBoolean(p1.value < p2.value))
      case NILessEqual(p1: LDouble, p2: LDouble) => List(LBoolean(p1.value <= p2.value))
      case NILessEqual(p1: LInteger, p2: LInteger) => List(LBoolean(p1.value <= p2.value))
      case NILessEqual(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value <= p2.value))
      case NILessEqual(p1: LCharacter, p2: LCharacter) => List(LBoolean(p1.value <= p2.value))
      case NIEqual(p1: LDouble, p2: LDouble) => List(LBoolean(p1.value == p2.value))
      case NIEqual(p1: LInteger, p2: LInteger) => List(LBoolean(p1.value == p2.value))
      case NIEqual(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value == p2.value))
      case NIEqual(p1: LCharacter, p2: LCharacter) => List(LBoolean(p1.value == p2.value))
      case NINotEqual(p1: LDouble, p2: LDouble) => List(LBoolean(p1.value != p2.value))
      case NINotEqual(p1: LInteger, p2: LInteger) => List(LBoolean(p1.value != p2.value))
      case NINotEqual(p1: LBoolean, p2: LBoolean) => List(LBoolean(p1.value != p2.value))
      case NINotEqual(p1: LCharacter, p2: LCharacter) => List(LBoolean(p1.value != p2.value))
      case NIIsError(p1: LError, p2: LError) => List(LBoolean(p1.text == p2.text))
      case NIBool(p1: LInteger) => List(UtilitySNode.integerToBool(p1.value))
      case NIChar(p1: LInteger) => List(LCharacter(p1.value.toChar.toString))
      case NIDouble(p1: LDouble) => List(LDouble(p1.value))
      case NIDouble(p1: LInteger) => List(LDouble(p1.value))
      case NIInt(p1: LDouble) => List(LInteger(Math.floor(p1.value + 0.5).toInt))
      case NIInt(p1: LInteger) => List(LInteger(Math.floor(p1.value + 0.5).toInt))
      case NIInt(p1: LCharacter) => List(LInteger(p1.value.toCharArray()(0).toInt))
      case NIInt(p1: LBoolean) => List(UtilitySNode.boolToInteger(p1.value))
      case NIFloor(p1: LDouble) => List(LInteger(Math.floor(p1.value).toInt))
      case NITrunc(p1: LDouble) => List(LInteger(p1.value.toInt))
      case NISingle(p1: LDouble) => List(LDouble(p1.value))
      case NISingle(p1: LInteger) => List(LDouble(p1.value))
      case NINoOp(inputs: Seq[Literal]) => inputs
      case NIABuild(p1: LInteger, inputs: Seq[Literal]) => List(LArray(inputs))
      case NIAFill(p1: LInteger, p2: LInteger, p3: Literal) => List(LArray(List.make(p2.value - 1, p3)))
      case NIAElement(p1: LArray, p2: LInteger) => List(p1.values(p2.value - 1))
      case NIAReplace(p1: LArray, p2: LInteger, inputs: Seq[Literal]) => List(LArray(p1.values.slice(p2.value - 1, p1.values.size - 1) ++ inputs))
      case NIACatenate(p1: LArray, inputs: Seq[Literal]) => List(LArray(UtilitySNode.catenate(p1, inputs.collect { case x: LArray => x })))
      case NIALimL(p1: LArray) => List(LInteger(1))
      case NIALimH(p1: LArray) => List(LInteger(p1.values.size))
      case NIASize(p1: LArray) => List(LInteger(UtilitySNode.maxArraySize(p1)))
      case NIAIsEmpty(p1: LArray) => List(LBoolean(p1.values.isEmpty))
      case NIAAddL(p1: LArray, p2: Literal) => List(LArray(p2 :: p1.values.toList))
      case NIAAddH(p1: LArray, p2: Literal) => List(LArray(p1.values.toList :+ p2))
      case NIARemL(p1: LArray) => List(LArray(p1.values.slice(1, p1.values.size - 1)))
      case NIARemH(p1: LArray) => List(LArray(p1.values.slice(0, p1.values.size - 2)))
      case NIASetL(p1: LArray, p2: LInteger) => List(p1)
      case NIAExtract(p1: LArray, p2: LInteger, p3: LInteger) => List(LArray(p1.values.slice(p2.value - 1, p3.value)))
      case NIRBuild(inputs: Seq[Literal]) => List(LRecord(inputs))
      case NIRBuildUnion(p1: Literal) => List(LUnion(0, p1))
      case NIRElements(p1: LRecord) => p1.values
      case NIRReplace(p1: LRecord, inputs: Seq[Literal]) => List(UtilitySNode.replace(p1, inputs))
      case NIAScatter(p1: LMultiple) => List(LMultiple(p1.values), LMultiple(p1.values.zipWithIndex.map(x => LInteger(x._2 + 1))))
      case NIRangeGenerate(p1: LInteger, p2: LInteger) => List(LMultiple(List.range(p1.value, p2.value + 1, 1).map(x => LInteger(x))))
      case NIAGather(p1: LInteger, p2: LMultiple, Some(p3: LMultiple)) => List(LArray(p2.values.zip(p3.values).filter(_._2 == LBoolean(true)).map(x => x._1)))
      case NIAGather(p1: LInteger, p2: LMultiple, None) => List(LArray(p2.values))
      case NIReduce(p1: LString, p2: Literal, p3: LMultiple, None) => List(UtilitySNode.reduceLeft(p1, p2, p3))
      case NIReduce(p1: LString, p2: Literal, p3: LMultiple, Some(p4: LMultiple)) => List(UtilitySNode.reduceLeft(p1, p2, p3, p4))
      case NIRedLeft(p1: LString, p2: Literal, p3: LMultiple, None) => List(UtilitySNode.reduceLeft(p1, p2, p3))
      case NIRedLeft(p1: LString, p2: Literal, p3: LMultiple, Some(p4: LMultiple)) => List(UtilitySNode.reduceLeft(p1, p2, p3, p4))
      case NIRedRight(p1: LString, p2: Literal, p3: LMultiple, None) => List(UtilitySNode.reduceRight(p1, p2, p3))
      case NIRedRight(p1: LString, p2: Literal, p3: LMultiple, Some(p4: LMultiple)) => List(UtilitySNode.reduceRight(p1, p2, p3, p4))
      case NIRedTree(p1: LString, p2: Literal, p3: LMultiple, None) => List(UtilitySNode.reduceLeft(p1, p2, p3))
      case NIRedTree(p1: LString, p2: Literal, p3: LMultiple, Some(p4: LMultiple)) => List(UtilitySNode.reduceLeft(p1, p2, p3, p4))
      case NIFirstValue(p1: LMultiple, Some(p2: LMultiple)) => List(UtilitySNode.filter(p1, p2)(0))
      case NIFirstValue(p1: LMultiple, None) => List(p1.values(0))
      case NIFirstValue(p1: Literal, None) => List(p1)
      case NIFinalValue(p1: LMultiple, Some(p2: LMultiple)) => List(UtilitySNode.filter(p1, p2)(p1.values.size - 1))
      case NIFinalValue(p1: LMultiple, None) => List(p1.values(p1.values.size - 1))
      case NIFinalValue(p1: Literal, None) => List(p1)
      case NICall(p1: LString, inputs: Seq[Literal]) =>
        val graph = Runner.program.graphs.find(x => x.name == p1).get
        evalGraph(graph, inputs)
      case NIBindArguments(p1: LString, inputs: Seq[Literal]) => ???
      case n: NSelect => UtilityCNode.evalSelect(n, values)
      case n: NTagCase => UtilityCNode.evalTagCase(n, values)
      case n: NForAll => UtilityCNode.evalForAll(n, values)
      case n: NLoopA => UtilityCNode.evalLoopA(n, values)
      case n: NLoopB => UtilityCNode.evalLoopB(n, values)
      case NIPlus(v1, v2) => List(LError("Plus: Wrong types", v1, v2))
      case NITimes(p1, p2) => List(LError("Times: Wrong types", p1, p2))
      case NIMin(p1, p2) => List(LError("Min: Wrong types", p1, p2))
      case NIMax(p1, p2) => List(LError("Max: Wrong types", p1, p2))
      case NIMinus(v1, v2) => List(LError("Minus: Wrong types", v1, v2))
      case NIDiv(p1, p2) => List(LError("Div: Wrong types", p1, p2))
      case NIMod(p1, p2) => List(LError("Mod: Wrong types", p1, p2))
      case NIExp(p1, p2) => List(LError("Exp: Wrong types", p1, p2))
      case NIAbs(p1) => List(LError("Abs: Wrong types", p1))
      case NINeg(p1) => List(LError("Neg: Wrong types", p1))
      case NINot(p1) => List(LError("Not: Wrong types", p1))
      case NILess(p1, p2) => List(LError("Less: Wrong types", p1, p2))
      case NILessEqual(p1, p2) => List(LError("LessEqual: Wrong types", p1, p2))
      case NIEqual(p1, p2) => List(LError("Equal: Wrong types", p1, p2))
      case NINotEqual(p1, p2) => List(LError("NotEqual: Wrong types", p1, p2))
      case NIIsError(p1, p2) => List(LError("IsError: Wrong types. Expected both arguments to be of type error", p1, p2))
      case NIBool(p1) => List(LError("Bool: Wrong type. Expected type of integer", p1))
      case NIChar(p1) => List(LError("Char: Wrong type. Expected type of integer", p1))
      case NIDouble(p1) => List(LError("Double: Wrong type. Expected type of integer or double", p1))
      case NIInt(p1) => List(LError("Int: Wrong type"))
      case NIFloor(p1) => List(LError("Floor: Wrong type. Expected type of double", p1))
      case NITrunc(p1) => List(LError("Trunc: Wrong type. Expected type of double", p1))
      case NISingle(p1) => List(LError("Single: Wrong type. Expected type of integer or double", p1))
      case NIABuild(p1, inputs) => List(LError("ABuild: Wrong type: Expected type of integer"))
      case NIAFill(p1, p2, p3) => List(LError("AFill: Wrong types. Expected both types to be of integer", p1, p2, p3))
      case NIAElement(p1, p2) => List(LError("AElement: Wrong types. Expected array type and integer type", p1, p2))
      case NIAReplace(p1, p2, inputs) => List(LError("AReplace: Wrong types. Expected array type and integer type", p1, p2))
      case NIACatenate(p1, inputs) => List(LError("ACatenate: Wrong type. Expected array type", p1))
      case NIALimL(p1) => List(LError("ALimL: Expected type of array", p1))
      case NIALimH(p1) => List(LError("ALimH: Expected type of array", p1))
      case NIASize(p1) => List(LError("ASize: Wrong type. Expected type of array", p1))
      case NIAIsEmpty(p1) => List(LError("AIsEmpty: Wrong type. Expected type of array", p1))
      case NIAAddL(p1, p2) => List(LError("AAddL: Wrong types. Expected first argument to be of type array", p1, p2))
      case NIAAddH(p1, p2) => List(LError("AAddH: Wrong types. Expected first argument to be of type array", p1, p2))
      case NIARemL(p1) => List(LError("ARemL: Wrong type. Expected type of array", p1))
      case NIARemH(p1) => List(LError("ARemH: Wrong type. Expected type of array", p1))
      case NIASetL(p1, p2) => List(LError("ASetL: Wrong type. Expected type of array", p1, p2))
      case NIAExtract(p1, p2, p3) => List(LError("AExtract: Wrong types, Expected type of array, integer and integer", p1, p2, p3))
      case NIRBuild(inputs) => List(LError("RBuild: Wrong type. Expected a number of arguments")) //???
      case NIRElements(p1) => List(LError("NRelements: Wrong type. Expected type of record", p1))
      case NIRReplace(p1, inputs) => List(LError("NRReokace: Wrong type. Expected type of record", p1))
      case NIAScatter(p1) => List(LError("NAScatter: Wrong type. Expected type of multiple", p1))
      case NIRangeGenerate(p1: Literal, p2: Literal) => List(LError("NRangeGenerate: Wrong types. Expected both arguments to be of type integer", p1, p2))
      case NIAGather(p1, p2, Some(p3)) => List(LError("AGather: Wrong types. Expected arguments to be of type integer and of type array", p1, p2))
      case NIAGather(p1, p2, None) => List(LError("AGather: Wrong types. Expected arguments to be of type integer and of type multiple", p1, p2))
      case NIReduce(p1, p2, p3, Some(p4)) => List(LError("NReduce: Wrong types", p1, p2, p3, p4))
      case NIReduce(p1, p2, p3, None) => List(LError("NReduce: Wrong types", p1, p2, p3))
      case NIRedLeft(p1, p2, p3, Some(p4)) => List(LError("NRedLeft: Wrong types", p1, p2, p3, p4))
      case NIRedLeft(p1, p2, p3, None) => List(LError("NRedLeft: Wrong types", p1, p2, p3))
      case NIRedRight(p1, p2, p3, Some(p4: LMultiple)) => List(LError("NRedRight: Wrong types", p1, p2, p3, p4))
      case NIRedRight(p1, p2, p3, None) => List(LError("NRedRight: Wrong types", p1, p2, p3))
      case NIRedTree(p1, p2, p3, Some(p4)) => List(LError("NRedTree: Wrong types", p1, p2, p3, p4))
      case NIRedTree(p1, p2, p3, None) => List(LError("NRedTree: Wrong types", p1, p2, p3))
      case NIFirstValue(p1, Some(p2)) => List(LError("NFirstValue: Wrong types", p1, p2))
      case NIFirstValue(p1, None) => List(LError("NFirstValue: Wrong type", p1))
      case NIFinalValue(p1, Some(p2)) => List(LError("NFinalValue: Wrong types", p1, p2))
      case NIFinalValue(p1, None) => List(LError("NFinalValue: Wrong type", p1))
      case NICall(p1, inputs) => List(LError("NCall: Wrong type. Expected function type", p1))
      case NIBindArguments(p1, inputs) => List(LError("NBindArguments: Wrong type. Expected function type", p1))
    }
  }
}