package parser

import parser.Type._
import parser._

case class NForAll(id:Int, graphs:Graphs, associations:Seq[Int], meta:PMetaInfo) extends PCNode
case class NSelect(id:Int, graphs:Graphs, associations:Seq[Int], meta:PMetaInfo) extends PCNode
case class NTagCase(id:Int, graphs:Graphs, associations:Seq[Int], meta:PMetaInfo) extends PCNode
case class NLoopA(id:Int, graphs:Graphs, associations:Seq[Int], meta:PMetaInfo) extends PCNode
case class NLoopB(id:Int, graphs:Graphs, associations:Seq[Int], meta:PMetaInfo) extends PCNode

case class NEmpty(id:Int, meta:PMetaInfo) extends PSNode
case class NAAddH(id:Int, meta:PMetaInfo) extends PSNode
case class NAAddL(id:Int, meta:PMetaInfo) extends PSNode
case class NAExtract(id:Int, meta:PMetaInfo) extends PSNode
case class NABuild(id:Int, meta:PMetaInfo) extends PSNode
case class NACatenate(id:Int, meta:PMetaInfo) extends PSNode
case class NAElement(id:Int, meta:PMetaInfo) extends PSNode
case class NAFill(id:Int, meta:PMetaInfo) extends PSNode
case class NAGather(id:Int, meta:PMetaInfo) extends PSNode
case class NAIsEmpty(id:Int, meta:PMetaInfo) extends PSNode
case class NALimH(id:Int, meta:PMetaInfo) extends PSNode
case class NALimL(id:Int, meta:PMetaInfo) extends PSNode
case class NARemH(id:Int, meta:PMetaInfo) extends PSNode
case class NARemL(id:Int, meta:PMetaInfo) extends PSNode
case class NAReplace(id:Int, meta:PMetaInfo) extends PSNode
case class NAScatter(id:Int, meta:PMetaInfo) extends PSNode
case class NASetL(id:Int, meta:PMetaInfo) extends PSNode
case class NASize(id:Int, meta:PMetaInfo) extends PSNode
case class NAbs(id:Int, meta:PMetaInfo) extends PSNode
case class NBindArguments(id:Int, meta:PMetaInfo) extends PSNode
case class NBool(id:Int, meta:PMetaInfo) extends PSNode
case class NCall(id:Int, meta:PMetaInfo) extends PSNode
case class NChar(id:Int, meta:PMetaInfo) extends PSNode
case class NDiv(id:Int, meta:PMetaInfo) extends PSNode
case class NDouble(id:Int, meta:PMetaInfo) extends PSNode
case class NEqual(id:Int, meta:PMetaInfo) extends PSNode
case class NExp(id:Int, meta:PMetaInfo) extends PSNode
case class NFirstValue(id:Int, meta:PMetaInfo) extends PSNode
case class NFinalValue(id:Int, meta:PMetaInfo) extends PSNode
case class NFloor(id:Int, meta:PMetaInfo) extends PSNode
case class NInt(id:Int, meta:PMetaInfo) extends PSNode
case class NIsError(id:Int, meta:PMetaInfo) extends PSNode
case class NLess(id:Int, meta:PMetaInfo) extends PSNode
case class NLessEqual(id:Int, meta:PMetaInfo) extends PSNode
case class NMax(id:Int, meta:PMetaInfo) extends PSNode
case class NMin(id:Int, meta:PMetaInfo) extends PSNode
case class NMinus(id:Int, meta:PMetaInfo) extends PSNode
case class NMod(id:Int, meta:PMetaInfo) extends PSNode
case class NNeg(id:Int, meta:PMetaInfo) extends PSNode
case class NNoOp(id:Int, meta:PMetaInfo) extends PSNode
case class NNot(id:Int, meta:PMetaInfo) extends PSNode
case class NNotEqual(id:Int, meta:PMetaInfo) extends PSNode
case class NPlus(id:Int, meta:PMetaInfo) extends PSNode
case class NRangeGenerate(id:Int, meta:PMetaInfo) extends PSNode
case class NRBuild(id:Int, meta:PMetaInfo) extends PSNode
case class NRElements(id:Int, meta:PMetaInfo) extends PSNode
case class NRReplace(id:Int, meta:PMetaInfo) extends PSNode
case class NRedLeft(id:Int, meta:PMetaInfo) extends PSNode
case class NRedRight(id:Int, meta:PMetaInfo) extends PSNode
case class NRedTree(id:Int, meta:PMetaInfo) extends PSNode
case class NReduce(id: Int, meta:PMetaInfo) extends PSNode
case class NRestValues(id:Int, meta:PMetaInfo) extends PSNode
case class NSingle(id:Int, meta:PMetaInfo) extends PSNode
case class NTimes(id:Int, meta:PMetaInfo) extends PSNode
case class NTrunc(id:Int, meta:PMetaInfo) extends PSNode
