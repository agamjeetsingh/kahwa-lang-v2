package diagnostics

import sources.SourceRange

enum Diagnostic(val msg: String, val range: SourceRange) {
  case UnrecognisedToken(token: String, override val range: SourceRange) extends Diagnostic(s"Unrecognised token '$token'", range)
  
  case UnterminatedStringLiteral(override val range: SourceRange) extends Diagnostic(s"Unterminated string literal", range)
  
  case ExpectedSomething(expected: String, found: String, override val range: SourceRange) extends Diagnostic(s"Expected '$expected' but found '$found'", range)
  
  case SymbolAlreadyDeclared(name: String, override val range: SourceRange) extends Diagnostic(s"Symbol '$name' has already been declared in the current scope", range)
}