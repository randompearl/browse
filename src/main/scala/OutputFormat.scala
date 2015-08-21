/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import java.io.File

/** Enumerates the output formats handled by the plugin. */
object OutputFormat extends Enumeration {
	type OutputFormat = Value

	// The enumeration values
	val Html = Value("html")
	val Vim = Value("vim")
  val Text = Value("text")
	def all: List[OutputFormat] = Html :: Vim :: Text :: Nil

	private[this] type Factory = OutputWriterContext => OutputWriter
	private[this] def factory(format: OutputFormat): Factory = format match {
		case Html => new HtmlWriter(_)
		case Vim => new vim.VimWriter(_)
    case Text => new text.TextWriter(_)
	}

	/** Returns the writer corresponding to a value, configured with a context */
	def getWriter(value: OutputFormat, context: OutputWriterContext): OutputWriter =
		factory(value)(context)
}
