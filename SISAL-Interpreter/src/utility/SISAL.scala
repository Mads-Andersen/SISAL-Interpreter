package utility

import sys.process._
import java.io._
import scala.sys.process.Process
import parser.Parser
import interpreter.Interpreter
import parser.Type._

object SISAL {

  def compileString(input: String) = {
    val path = "../Programs/Temp.sis"
    val file = new File(path)
    val writer = new PrintWriter(file)
    writer.write(input)
    writer.close()
    compileFile(path)
  }
  
  def compileFunction(input: String) = {
    val code = s"""define main\n $input \n end function % main"""
    SISAL.compileString(code)
  }
  
  def compileFile(path: String) = {
    val pathIf1 = path.replaceAll("\\.[^.]*$", "") + ".if1"
    Process("/usr/local/bin/sisalc", Seq("-IF1", path))! ;
    val objects = Parser.createParseGraphFromFile(pathIf1)
    objects.collect { case x: Graph => x }
  }
  
  def compileIF1File(path: String) = {
    Parser.createParseGraphFromFile(path)
  }
}