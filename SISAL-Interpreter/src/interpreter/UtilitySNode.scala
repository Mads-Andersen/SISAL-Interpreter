package interpreter

import parser._

object UtilitySNode {

  def boolToInteger(value: Boolean) = value match {
    case false => LInteger(0)
    case true => LInteger(1)
  }
  
  def integerToBool(value: Int) = value match {
    case 0 => LBoolean(false)
    case 1 => LBoolean(true)
    case _ => LError("Bool: Expected the integer 1 or 0")
  }
  
  def filter(m1: LMultiple, m2: LMultiple) = {
     val filtered = m1.values.zip(m2.values).filter(x => x._2 == LBoolean(true))
     val selected = filtered.map(x => x._1)
     selected
  }
  
  def replace(record: LRecord, inputs: Seq[Literal]) = {
    val fields = record.values
    val zipped = fields.zip(inputs)
    def replace(v1: Literal, v2: Literal) = v2 match {
      case x: LEmpty => v1
      case _ => v2
    }
    LRecord(zipped.map(x => replace(x._1, x._2)))
  }
  
  def catenate(p1: LArray, p2: Seq[LArray]) = {
    def doCatenate(arrays: Seq[LArray], values: Seq[Literal]) : Seq[Literal] = arrays match {
      case Nil => values
      case h :: t => doCatenate(t, h.values ++ values)
    }
    p1.values ++ doCatenate(p2, List())
  }  
  
  def maxArraySize(arrays: LArray) : Int = maxArraySize(arrays, 0)
  private def maxArraySize(arrays: LArray, maxDimension: Int) : Int = {
    val nextDimension = arrays.values.collect{case x: LArray => x}
    val nextMax = maxArraySize(LArray(nextDimension), maxDimension)
    val size = arrays.values.size
    if(size >= nextMax) size else nextMax
  }
  
  type Fold[A] = (Seq[Literal], A, (Literal, A) => A) => A
  
  def reduceLeft(function: LString, startValue: Literal, multiple: LMultiple) : Literal = {
    def fold[A](xs: Seq[Literal], base: Literal, func: (Literal, Literal) => Literal) = xs.foldLeft(base) { (a,current) => func(a,current) }
    genReduce(fold, function, startValue, multiple)
  }
  
  def reduceRight(function: LString, startValue: Literal, multiple: LMultiple) : Literal = {
    def fold[A](xs: Seq[Literal], base: Literal, func: (Literal, Literal) => Literal) = xs.foldRight(base){ (current, a) => func(a,current) }
    genReduce(fold, function, startValue, multiple)
  }
  
  def reduceTree(function: LString, startValue: Literal, multiple: LMultiple) : Literal = {
    def fold[A](xs: Seq[Literal], base: Literal, func: (Literal, Literal) => Literal) = foldTree(xs, base, func)
    genReduce(fold, function, startValue, multiple)
  }
  
  def reduceLeft(function: LString, startValue: Literal, multiple: LMultiple, booleans: LMultiple) : Literal = {
    val values = multiple.values.zip(booleans.values).filter(x => x._2 == LBoolean(true)).map(x => x._1)
    reduceLeft(function, startValue, LMultiple(values))
  }
  
  def reduceRight(function: LString, startValue: Literal, multiple: LMultiple, booleans: LMultiple) : Literal = {
    val values = multiple.values.zip(booleans.values).filter(x => x._2 == LBoolean(true)).map(x => x._1)
    reduceRight(function, startValue, LMultiple(values))
  }
  
  def reduceTree(function: LString, startValue: Literal, multiple: LMultiple, booleans: LMultiple) : Literal = {
    val values = multiple.values.zip(booleans.values).filter(x => x._2 == LBoolean(true)).map(x => x._1)
    reduceTree(function, startValue, LMultiple(values))
  }
  
  private def foldTree[Literal](xs: Seq[Literal], base: Literal, func: (Literal, Literal) => Literal): Literal = {
    if(xs.size >= 2){
      val left = xs.slice(0, xs.size/2)
      val right = xs.slice(xs.size/2, xs.size)
      val t1 = foldTree(left, base, func)
      val t2 = foldTree(right, base, func)
      func(t1, t2)
    }
    else
    {
      xs(0)
    }
  }

  private def genReduce(fold: Fold[Literal], f: LString, startValue: Literal, multiple: LMultiple) : Literal = {
    val values = multiple.values
    val value = f.value.toUpperCase() match {
      case "SUM"      => fold(values, startValue, sum)
      case "PRODUCT"  => fold(values, startValue, product)
      case "LEAST"    => fold(values, startValue, least)
      case "GREATEST" => fold(values, startValue, greatest)
      case "CATENATE" => fold(values, startValue, catenate)
    }
    value
  }
    
  def sum(total: Literal, cur: Literal) = (total, cur) match {
    case (x: LInteger, y: LInteger) => LInteger(x.value + y.value)
    case (x: LDouble, y: LDouble) => LDouble(x.value + y.value)
    case (x: LBoolean, y: LBoolean) => LBoolean(x.value | y.value)
    case (_,_) => total
  }
  
  private def product(total: Literal, cur: Literal) = (total, cur) match {
    case (x: LInteger, y: LInteger) => LInteger(x.value * y.value)
    case (x: LDouble, y: LDouble) => LDouble(x.value * y.value)
    case (x: LBoolean, y: LBoolean) => LBoolean(x.value & y.value)
    case (_,_) => total
  }
  
  private def least(total: Literal, cur: Literal) = (total, cur) match {
    case (x: LInteger, y: LInteger) => if(y.value < x.value) LInteger(y.value) else LInteger(x.value)
    case (x: LDouble, y: LDouble) => if(y.value < x.value) LDouble(y.value) else LDouble(x.value)
    case (x: LBoolean, y: LBoolean) => if(y.value < x.value) LBoolean(y.value) else LBoolean(x.value)
    case (_,_) => total
  }
  
  private def greatest(total: Literal, cur: Literal) = (total, cur) match {
    case (x: LInteger, y: LInteger) => if(y.value > x.value) LInteger(y.value) else LInteger(x.value)
    case (x: LDouble, y: LDouble) => if(y.value > x.value) LDouble(y.value) else LDouble(x.value)
    case (x: LBoolean, y: LBoolean) => if(y.value > x.value) LBoolean(y.value) else LBoolean(x.value)
    case (_,_) => total
  }
  
  private def catenate(total: Literal, cur: Literal) = (total, cur) match {
    case (x: LArray, y: LArray) => LArray(x.values ++ y.values)
    case (_, _ ) => total
  }
}