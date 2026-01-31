package diagnostics

import sources.SourceRange

enum Diagnostic(val msg: String, val range: SourceRange) {
  case UnrecognisedToken(token: String, override val range: SourceRange) extends Diagnostic(s"Unrecognised token '$token'", range)
  
  case UnterminatedStringLiteral(override val range: SourceRange) extends Diagnostic(s"Unterminated string literal", range)
  
  case ExpectedSomething(expected: String, found: String, override val range: SourceRange) extends Diagnostic(s"Expected '$expected' but found '$found'", range)
}