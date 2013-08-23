package interpreter

import parser._
import parser.Type._

object NIPlus {
  def unapply(n: NPlus)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NITimes {
  def unapply(n: NTimes)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIMin {
  def unapply(n: NMin)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIMax {
  def unapply(n: NMax)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIMinus {
  def unapply(n: NMinus)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIDiv {
  def unapply(n: NDiv)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIMod {
  def unapply(n: NMod)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIExp {
  def unapply(n: NExp)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIAbs {
  def unapply(n: NAbs)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NINeg {
  def unapply(n: NNeg)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NINot {
  def unapply(n: NNot)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NILess {
  def unapply(n: NLess)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NILessEqual {
  def unapply(n: NLessEqual)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIEqual {
  def unapply(n: NEqual)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NINotEqual {
  def unapply(n: NNotEqual)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIIsError {
  def unapply(n: NIsError)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some((values(0), values(1)))
  }
}
object NIBool {
  def unapply(n: NBool)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIChar {
  def unapply(n: NChar)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIDouble {
  def unapply(n: NDouble)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIInt {
  def unapply(n: NInt)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIFloor {
  def unapply(n: NFloor)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NITrunc {
  def unapply(n: NTrunc)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NISingle {
  def unapply(n: NSingle)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NINoOp {
  def unapply(n: NNoOp)(implicit env: Environment, graph: Graph): Option[(List[Literal])] = {
    val values = Node.getInputs(n, graph, env)
    Some(values.toList)
  }
}
object NIABuild {
  def unapply(n: NABuild)(implicit env: Environment, graph: Graph): Option[(Literal, List[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: LInteger) :: t => Some(p1, t)
      case _ => None
    }
  }
}
object NIAFill {
  def unapply(n: NAFill)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1), values(2))
  }
}
object NIAElement {
  def unapply(n: NAElement)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1))
  }
}
object NIAReplace {
  def unapply(n: NAReplace)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, List[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: Literal) :: (v2: Literal) :: t => Some(v1, v2, t)
      case _ => None
    }
  }
}
object NIACatenate {
  def unapply(n: NACatenate)(implicit env: Environment, graph: Graph): Option[(Literal, List[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: Literal) :: t => Some(v1, t)
      case _ => None
    }
  }
}
object NIALimL {
  def unapply(n: NALimL)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIALimH {
  def unapply(n: NALimH)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIASize {
  def unapply(n: NASize)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIAIsEmpty {
  def unapply(n: NAIsEmpty)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIAAddL {
  def unapply(n: NAAddL)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1))
  }
}
object NIAAddH {
  def unapply(n: NAAddH)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1))
  }
}
object NIARemL {
  def unapply(n: NARemL)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIARemH {
  def unapply(n: NARemH)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIASetL {
  def unapply(n: NASetL)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1))
  }
}
object NIAExtract {
  def unapply(n: NAExtract)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1), values(2))
  }
}
object NIRBuild {
  def unapply(n: NRBuild)(implicit env: Environment, graph: Graph): Option[(Seq[Literal])] = {
    val values = Node.getInputs(n, graph, env)
    Some(values)
  }
}
object NIRBuildUnion {
  def unapply(n: NRBuild)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIRElements {
  def unapply(n: NRElements)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIRReplace {
  def unapply(n: NRReplace)(implicit env: Environment, graph: Graph): Option[(Literal, Seq[Literal])] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values.slice(1, values.size-1))
  }
}



object NIAScatter {
  def unapply(n: NAScatter)(implicit env: Environment, graph: Graph): Option[(Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0))
  }
}
object NIRangeGenerate {
  def unapply(n: NRangeGenerate)(implicit env: Environment, graph: Graph): Option[(Literal, Literal)] = {
    val values = Node.getInputs(n, graph, env)
    Some(values(0), values(1))
  }
}
object NIAGather {
  def unapply(n: NAGather)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: Literal) :: (p2: Literal) :: Nil => Some((p1, p2, None))
      case (p1: Literal) :: (p2: Literal) :: (p3: LMultiple) :: Nil => Some((p1, p2, Some(p3)))
      case _ => None
    }
  }
}
object NIReduce {
  def unapply(n: NReduce)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: Nil => Some((v1,v2,v3,None))
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: (v4: Literal) :: Nil => Some((v1,v2,v3,Some(v4)))
      case _ => None
    }
  }
}
object NIRedLeft {
  def unapply(n: NRedLeft)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: LString) :: (v2: Literal) :: (v3: Literal) :: Nil => Some((v1,v2,v3,None))
      case (v1: LString) :: (v2: Literal) :: (v3: Literal) :: (v4: LMultiple) :: Nil => Some((v1,v2,v3,Some(v4)))
      case _ => None
    }
  }
}
object NIRedRight {
  def unapply(n: NRedRight)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: Nil => Some((v1,v2,v3,None))
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: (v4: Literal) :: Nil => Some((v1,v2,v3,Some(v4)))
      case _ => None
    }
  }
}
object NIRedTree {
  def unapply(n: NRedTree)(implicit env: Environment, graph: Graph): Option[(Literal, Literal, Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: Nil => Some((v1,v2,v3,None))
      case (v1: Literal) :: (v2: Literal) :: (v3: Literal) :: (v4: Literal) :: Nil => Some((v1,v2,v3,Some(v4)))
      case _ => None
    }
  }
}
object NIFirstValue {
  def unapply(n: NFirstValue)(implicit env: Environment, graph: Graph): Option[(Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: Literal) :: Nil => Some((p1,None))
      case (p1: Literal) :: (p2: Literal) :: Nil => Some((p1,Some(p2)))
      case _ => None
    }
  }
}
object NIFinalValue {
  def unapply(n: NFinalValue)(implicit env: Environment, graph: Graph): Option[(Literal, Option[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: Literal) :: Nil => Some((p1,None))
      case (p1: Literal) :: (p2: Literal) :: Nil => Some((p1,Some(p2)))
      case _ => None
    }
  }
}
object NICall {
  def unapply(n: NCall)(implicit env: Environment, graph: Graph): Option[(Literal, Seq[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: Literal) :: t => Some((p1,t))
      case _ => None
    }
  }
}
object NIBindArguments{
  def unapply(n: NBindArguments)(implicit env: Environment, graph: Graph): Option[(Literal, Seq[Literal])] = {
    Node.getInputs(n, graph, env) match {
      case (p1: Literal) :: t => Some((p1,t))
      case _ => None
    }
  }
}