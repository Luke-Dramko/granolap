package granolap

import scala.io.Source
import java.io._


object Main {

  /**
    * This is the main entry point into the lexer/parser.
    * @param args Command line arguments. The first command line argument must be the file with a contract written in granola.
    */
  def main(args: Array[String]): Unit = {
    try {
      val file = new File(args(0))
      val handle = Source.fromFile(file)("utf-8")
      val program = handle.getLines.mkString("")
      handle.close()

      val output = GranolaParser(Lexer(program))

      //type checker goes here.

      println(output) //Perhaps serialize
      println("Operation completed successfully.")

    } catch {
      case e: FileNotFoundException => println("Error: cannot find input file " + args(0))
      case e: ArrayIndexOutOfBoundsException => println("Error: file argument not specified.")
      case e: ParsingException => println("An error was found during parsing: " + e.getMessage)
      case e: LexicalException => println("An error was found during lexing: " + e.getMessage)
    }

  }
}