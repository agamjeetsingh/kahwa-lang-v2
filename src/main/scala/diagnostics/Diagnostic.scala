package diagnostics

import sources.SourceRange

enum Diagnostic(val msg: String, val sourceRange: SourceRange) {
  case ExpectedSomething(expected: String, found: String, range: SourceRange) extends Diagnostic(s"Expected '$expected' but found '$found'", range)
}