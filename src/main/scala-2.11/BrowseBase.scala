package sxr

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import ast.parser.Tokens
import plugins.Plugin
import symtab.Flags
import reflect.internal.util.SourceFile

object TokenUtils {
  import Tokens.{COMMENT, USCORE,LPAREN, RBRACE, NEW, RETURN,  isIdentifier => isId, isLiteral => isLit}
  def isKeyword( code : Int ) = code >= NEW && code <= RETURN
  def isBrace( code : Int ) = code >= LPAREN && code <= RBRACE
  def isComment( code : Int ) = code == COMMENT || code == USCORE
  def isIdentifier( code : Int ) = isId( code )
  def isLiteral( code : Int ) = isLit( code )
}
abstract class BrowseBase extends Plugin {
  val global : Global
  import global._ 

	/** Filters out unwanted tokens such as whitespace and commas.  Braces are currently
	* included because () is annotated as Unit, and a partial function created by
	* { case ... } is associated with the opening brace.  */
	private def includeToken(code: Int) = { 
    import TokenUtils._
    isComment(code) || 
			 isKeyword(code) || isIdentifier(code) || isLiteral(code) || isBrace(code)
  }

  class Scan(unit : CompilationUnit) extends syntaxAnalyzer.UnitScanner(unit)
  {
		private[sxr] val tokens = wrap.Wrappers.treeSet[Token]
		def addComment(start: Int, end: Int) { tokens += new Token(start, end - start + 1, Tokens.COMMENT) }

    override def deprecationWarning(off: Int, msg: String) {}
    override def error(off: Int, msg: String) {}
    override def incompleteInputError(off: Int, msg: String) {}

    override def nextToken() {
      val offset0 = offset
      val code = token

      super.nextToken()

      if(includeToken(code)) {
        val length = (lastOffset - offset0) max 1
        tokens += new Token(offset0, length, code)
      }
    }

  }
}

