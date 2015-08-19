package sxr

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import ast.parser.Tokens
import plugins.Plugin
import symtab.Flags
import reflect.internal.util.SourceFile

object TokenUtils {
  import Tokens.{COMMENT, USCORE, isBrace => _isBrace, isKeyword => _isKeyword, isIdentifier => _isIdentifier, isLiteral => _isLiteral}
  def isBrace( code : Int ) = _isBrace( code )
  def isKeyword( code : Int ) = _isKeyword( code )
  def isIdentifier( code : Int ) = _isIdentifier( code )
  def isLiteral( code : Int ) = _isLiteral( code )
}

abstract class BrowseBase extends Plugin {
  val global : Global
  import global._ 

	/** Filters out unwanted tokens such as whitespace and commas.  Braces are currently
	* included because () is annotated as Unit, and a partial function created by
	* { case ... } is associated with the opening brace.  */
	private def includeToken(code: Int) =
	{
		import Tokens.{COMMENT, USCORE, isBrace, isKeyword, isIdentifier, isLiteral}
		code match
		{
			case COMMENT | USCORE => true
			case _ => isKeyword(code) || isIdentifier(code) || isLiteral(code) || isBrace(code)
		}
	}

  class Scan(unit : CompilationUnit) extends syntaxAnalyzer.UnitScanner(unit)
  {
		private[sxr] val tokens = wrap.Wrappers.treeSet[Token]
		def addComment(start: Int, end: Int) { tokens += new Token(start, end - start + 1, Tokens.COMMENT) }

    override def deprecationWarning(off: Int, msg: String) {}
    override def error(off: Int, msg: String) {}
    override def incompleteInputError(off: Int, msg: String) {}

    override def foundComment(value: String, start: Int, end: Int) {
      addComment(start, end)
      super.foundComment(value, start, end)
     }
    override def foundDocComment(value: String, start: Int, end: Int) {
      addComment(start, end)
      super.foundDocComment(value, start, end)
    }
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

