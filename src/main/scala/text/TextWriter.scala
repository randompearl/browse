package sxr.text

import sxr._

import java.io._
class TextWriter(context : OutputWriterContext) extends OutputWriter {

  val basedir = new File( context.outputDirectory, "summary" )
  def writeStart() {
  }
  def writeEnd() {
  }
  def writeUnit( sourceFile : File, relPath : String, tokens : List[Token] ) {
    val file = new File( basedir, relPath + ".txt")
    file.getParentFile().mkdirs()
    FileUtil.withWriter( file ) { writer =>
      tokens.foreach {
        case Token(start,length,code) => 
          writer.write(TokenUtils.toStr(code) + "::" + start + "::" + length)
          writer.newLine()
      }
    }
  }
}


