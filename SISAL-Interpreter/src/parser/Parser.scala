package parser

import scala.util.parsing.combinator.RegexParsers
import java.io.FileReader


object Parser extends RegexParsers 
{     
	override val whiteSpace = """( |\t)*""".r
	def Comment = """[^\n]*""".r
	def Text = """[^\n]*""".r
	def NewLine = """\n""".r
	def ErrorValue = "Error"
	def Int = """-?[0-9]+""".r ^^ {case n => n.toInt}
  
	def File = NewLine.* ~> Line.*
	def Line:Parser[PComponent] = (
		  GraphComponent
	    | LocalGraph
	    | "X" ~> TypeReference ~ ("\"" ~> String <~ "\"") ~ MetaInfo ~ NewLine ~ GraphComponent.+ ^^ {case r~l~i~_~c => PLGraph(l,r,c, i)}
	    | "I" ~> TypeReference ~ ("\"" ~> String <~ "\"") ~ MetaInfo <~ NewLine ^^ {case r~l~i => PIGraph(l,r,i)}
	)
	
	def GraphComponent:Parser[PComponent] = (
		  "C" ~> MetaInfo <~ NewLine ^^ {case i => new PComment(i)}
	    | "E" ~> Source ~ Destination ~ TypeReference ~ MetaInfo <~ NewLine ^^ {case s~d~r~i => PREdge(s._1, s._2, d._1, d._2, r, i)}
	    | "L" ~> Destination ~ TypeReference ~ ("\"" ~> Literal <~ "\"") ~ MetaInfo <~ NewLine ^^ {case d~r~l~i => PLEdge(Constant.LiteralSourceNode, Constant.LiteralSourcePort, d._1, d._2, r, l, i)}
	    | "N" ~> Label ~ Int ~ MetaInfo <~ NewLine ^^ {case l~n~i => getNode(l, n, i)}														
	    | "T" ~> Label ~ TypeTapleEntry ~ MetaInfo <~ NewLine ^^ {case l~t~i => PType(l,t,i)}
	    | "{" ~> (MetaInfo ~> NewLine ~> LocalGraph.* <~ "}") ~ Label ~ Int ~ (Int ~> AssocList) ~ (MetaInfo <~ NewLine) ^^ {case lines~l~n~al~i => getCompoundNode(l, n, lines, al, i)}
	)
	
	def LocalGraph:Parser[PLGraph] = (
	      "G" ~> TypeReference ~ ("\"" ~> String <~ "\"") ~ MetaInfo ~ NewLine ~ GraphComponent.+ ^^ {case r~l~i~_~c => PLGraph(l,r,c,i)}
	    | "G" ~> TypeReference ~ MetaInfo ~ NewLine ~ GraphComponent.+  ^^ {case r~i~_~c => PLGraph(LString(""),r,c, i)}
	)
	
	def MetaInfo = """[^(\n|%)]*""".r ~> MetaArg.* ^^ {x => new PMetaInfo(x)} 
	def MetaArg:Parser[MetaArg] = (ar | bd | lz | mk | na | op | sf | sl | st | xy)
	def ar = "%ar=" ~> Int ^^ {x => ARSize(x)}
	def bd = "%bd=" ~> Int ~ "," ~ Int ^^ {case l~x~h => Bounds(l,h)}
	def lz = "%lz=y" ^^ {x => ValueByDemand(true)} | "%lz=n" ^^ {x => ValueByDemand(false)}
	def mk = "%mk=V" ^^ {x => ValueOrRef(Mark.ValueMark)} | "%mk=R" ^^ {x => ValueOrRef(Mark.ReferenceMark)}
	def na = "%na=" ~> Text ^^ {x => Name(x)}
	def op = "%op=" ~> Int ^^ {x => OperationNr(x)}
	def sf = "%sf=" ~> Text ^^ {x => SourceFile(x)}
	def sl = "%sl=" ~> Int ^^ {x => SourceLine(x)}
	def st = "%st=p" ^^ {x => PointerOrContiguos(AllocationStyle.Pointer)} | "%st=c" ^^ {x => PointerOrContiguos(AllocationStyle.Contiguos)}
	def xy = "%xy=" ~> Int ~ "," ~ Int ^^ {case x~c~y => PositionXY(x,y)}
	  	
	def Label = Int
	def Source = Int ~ Int ^^ {case n ~ p => (n,p)}
	def Destination = Int ~ Int ^^ {case n ~ p => (n,p)}
	def TypeReference = Int
	def AssocList = Int.+
	
	def Literal:Parser[Literal] = ( Double | DoubleNoDot | Integer | Character | Null | Boolean | String | Record | Union )
	def Value = Literal	
	def PrintableCharacter = """[ -~]""".r
	def PrintableNonBacklash = """[ -\[]""".r
	def Integer = """-?[0-9]+""".r ^^ {case n => LInteger(n.toInt)}
	def DoubleNoDot = """-?[0-9]+(e|E)\-?[0-9]+""" ^^ {case d => LDouble(d.toDouble)}
	def Double = """-?[0-9]*\.[0-9]*(e|E)?\-?[0-9]+""".r ^^ {case d => LDouble(d.toDouble)}
	def Character = ("\'" ~ PrintableNonBacklash ~ "\'" ^^ {case _~c~_ => LCharacter(c.toString)} | "\\" ~ PrintableCharacter ^^ {case _~c => LCharacter(c.toString)})
	def Boolean = ("true" ^^ {case b => LBoolean(true)} | "false" ^^ {case b => LBoolean(false)})	
	def String = NoQuotes | Quotes 
	def NoQuotes = """[^\n\"]+""".r ^^ {x => LString(x)}
	def Quotes = """\"[^\n\"]+\"""".r ^^ {x => LString(x)}
	def StringOrBoolean = """[a-zA-Z0-9 ]+""".r ^^ {x => if(x.equals("true")) LBoolean(true) else if(x.equals("false")) LBoolean(false) else LString(x)}
	def Record = "<" ~> Value.+ <~ ">" ^^ {case v => LRecord(v)}
	def Union = "(" ~ Int ~ ":" ~ Value ~ ")" ^^ {case _ ~ i ~ _ ~ v ~ _ => LUnion(i,v)}
	def Null = "NIL" ^^ {case n => LNull()}  
	
	def TypeTapleEntry = (
	      Int ~ Int ~ Int ^^ {case x1~x2~x3 => getType(x1, x2, x3)}
	    | Int ~ Int ^^ {case x1~x2 => getType(x1,x2)}
	    | Int ^^ {case x => TArray(999)}
	)
	
	def getType(x:Int, arg1:Int, arg2:Int = 0) = x match {
	  	case 0 => TArray(arg1)
	  	case 1 => TBasic(getBasicType(arg1))
	  	case 2 => TField(arg1, arg2)
	  	case 3 => TFunction(arg1, arg2)
	  	case 4 => TMultiple(arg1)
	  	case 5 => TRecord(arg1)
	  	case 6 => TStream(arg1)
	  	case 7 => TTag(arg1, arg2)
	  	case 8 => TTuple(arg1, arg2)
	  	case 9 => TUnion(arg1)
	}
	
	def getBasicType(x:Int) = x match {
	 	case 0 => TBoolean
	 	case 1 => TCharacter
	 	case 2 => TDouble
	 	case 3 => TInteger
	 	case 4 => TNull
	 	case 5 => TReal
	 	case 6 => TWildBasic
	}

  def getNode(id: Int, nodeType: Int, meta:PMetaInfo) = nodeType match {
    case 100 => NAAddH(id, meta)
    case 101 => NAAddL(id, meta)
    case 102 => NAExtract(id, meta)
    case 103 => NABuild(id, meta)
    case 104 => NACatenate(id, meta)
    case 105 => NAElement(id, meta)
    case 106 => NAFill(id, meta)
    case 107 => NAGather(id, meta)
    case 108 => NAIsEmpty(id, meta)
    case 109 => NALimH(id, meta)
    case 110 => NALimL(id, meta)
    case 111 => NARemH(id, meta)
    case 112 => NARemL(id, meta)
    case 113 => NAReplace(id, meta)
    case 114 => NAScatter(id, meta)
    case 115 => NASetL(id, meta)
    case 116 => NASize(id, meta)
    case 117 => NAbs(id, meta)
    case 118 => NBindArguments(id, meta)
    case 119 => NBool(id, meta)
    case 120 => NCall(id, meta)
    case 121 => NChar(id, meta)
    case 122 => NDiv(id, meta)
    case 123 => NDouble(id, meta)
    case 124 => NEqual(id, meta)
    case 125 => NExp(id, meta)
    case 126 => NFirstValue(id, meta)
    case 127 => NFinalValue(id, meta)
    case 128 => NFloor(id, meta)
    case 129 => NInt(id, meta)
    case 130 => NIsError(id, meta)
    case 131 => NLess(id, meta)
    case 132 => NLessEqual(id, meta)
    case 133 => NMax(id, meta)
    case 134 => NMin(id, meta)
    case 135 => NMinus(id, meta)
    case 136 => NMod(id, meta)
    case 137 => NNeg(id, meta)
    case 138 => NNoOp(id, meta)
    case 139 => NNot(id, meta)
    case 140 => NNotEqual(id, meta)
    case 141 => NPlus(id, meta)
    case 142 => NRangeGenerate(id, meta)
    case 143 => NRBuild(id, meta)
    case 144 => NRElements(id, meta)
    case 145 => NRReplace(id, meta)
    case 146 => NRedLeft(id, meta)
    case 147 => NRedRight(id, meta)
    case 148 => NRedTree(id, meta)
    case 149 => NReduce(id, meta)
    case 150 => NRestValues(id, meta)
    case 151 => NSingle(id, meta)
    case 152 => NTimes(id, meta)
    case 153 => NTrunc(id, meta)
    case _ => NEmpty(id, meta)
  }
	
	def getCompoundNode(id: Int, nodeType: Int, graphs: Seq[PLGraph], associations: Seq[Int], meta:PMetaInfo) = nodeType match {
    case 0 => NForAll(id, graphs, associations, meta)
    case 1 => NSelect(id, graphs, associations, meta)
    case 2 => NTagCase(id, graphs, associations, meta)
    case 3 => NLoopA(id, graphs, associations, meta)
    case 4 => NLoopB(id, graphs, associations, meta)
	}
	
	def createParseGraphFromString(string: String) = {
	  parseAll(File, string).get
	}
	
	def createParseGraphFromFile(file: String) = {
	  val reader = new FileReader(file)
	  parseAll(File, reader).get
	}
	
	def createParseTypesFromFile(file: String) = {
	  val reader = new FileReader(file)
	  parseAll(File, reader).get
	}
	
	def createParseTypesFromString(string: String) = {
	  parseAll(File, string).get
	}
}