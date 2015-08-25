package sxr.text

import sxr._

import java.io._
class TextWriter(context : OutputWriterContext) extends OutputWriter {

  val basedir = new File( context.outputDirectory, "text" )
  def writeStart() {
  }
  def writeEnd() {
  }
  def writeUnit( sourceFile : File, relPath : String, tokens : List[Token] ) {
    val file = new File( basedir, relPath + ".txt")
    file.getParentFile().mkdirs()
    FileUtil.withWriter( file ) { writer =>
      tokens.foreach {
        case tok@Token(start,length,intCode) => 
          val code = TokenUtils.toStr(intCode)
          val tpe = tok.tpe.toString
          writer.write(s"""Token($code,$start,$length) -> $tpe""")
          writer.newLine()
      }
    }
  }
}


