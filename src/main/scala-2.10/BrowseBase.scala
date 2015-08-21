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
  def toStr( code : Int ) = {
    import Tokens._
    code match {
      case ABSTRACT => "ABSTRACT"
      case ARROW => "ARROW"
      case AT => "AT"
      case BACKQUOTED_IDENT => "BACKQUOTED_IDENT"
      case CASE => "CASE"
      case CASECLASS => "CASECLASS"
      case CASEOBJECT => "CASEOBJECT"
      case CATCH => "CATCH"
      case CLASS => "CLASS"
      case COLON => "COLON"
      case COMMA => "COMMA"
      case COMMENT => "COMMENT"
      case DEF => "DEF"
      case DO => "DO"
      case DOT => "DOT"
      case ELSE => "ELSE"
      case EQUALS => "EQUALS"
      case ESCAPE => "ESCAPE"
      case EXTENDS => "EXTENDS"
      case FALSE => "FALSE"
      case FINAL => "FINAL"
      case FINALLY => "FINALLY"
      case FOR => "FOR"
      case FORSOME => "FORSOME"
      case HASH => "HASH"
      case IDENTIFIER => "IDENTIFIER"
      case IF => "IF"
      case IGNORE => "IGNORE"
      case IMPLICIT => "IMPLICIT"
      case IMPORT => "IMPORT"
      case INTERPOLATIONID => "INTERPOLATIONID"
      case LARROW => "LARROW"
      case LAZY => "LAZY"
      case LBRACE => "LBRACE"
      case LBRACKET => "LBRACKET"
      case LPAREN => "LPAREN"
      case MACRO => "MACRO"
      case MATCH => "MATCH"
      case NEW => "NEW"
      case NEWLINE => "NEWLINE"
      case NEWLINES => "NEWLINES"
      case NULL => "NULL"
      case OBJECT => "OBJECT"
      case OVERRIDE => "OVERRIDE"
      case PACKAGE => "PACKAGE"
      case PRIVATE => "PRIVATE"
      case PROTECTED => "PROTECTED"
      case RBRACE => "RBRACE"
      case RBRACKET => "RBRACKET"
      case RETURN => "RETURN"
      case RPAREN => "RPAREN"
      case SEALED => "SEALED"
      case SEMI => "SEMI"
      case STRINGPART => "STRINGPART"
      case SUBTYPE => "SUBTYPE"
      case SUPER => "SUPER"
      case SUPERTYPE => "SUPERTYPE"
      case SYMBOLLIT => "SYMBOLLIT"
      case THEN => "THEN"
      case THIS => "THIS"
      case THROW => "THROW"
      case TRAIT => "TRAIT"
      case TRUE => "TRUE"
      case TRY => "TRY"
      case TYPE => "TYPE"
      case USCORE => "USCORE"
      case VAL => "VAL"
      case VAR => "VAR"
      case VIEWBOUND => "VIEWBOUND"
      case WHILE => "WHILE"
      case WHITESPACE => "WHITESPACE"
      case WITH => "WITH"
      case XMLSTART => "XMLSTART"
      case YIELD => "YIELD"
      case _ => 
        if(isOpenBrace(code))
          "OPEN_BRACE"
        else if(isCloseBrace(code))
          "CLOSE_BRACE"
        else if(this.isIdentifier(code))
          s"IDENTIFIER($code)"
        else if(this.isLiteral(code))
          s"LITERAL($code)"
        else if(this.isKeyword(code))
          s"KEYWORD($code)"
        else if(isSymbol(code))
          s"SYMBOL($code)"
        else
          "UNK(" + code + ")"
    }
  }
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

