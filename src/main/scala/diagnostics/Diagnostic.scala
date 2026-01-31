package diagnostics

import ast.Modifier
import sources.SourceRange

enum Diagnostic(val msg: String, val range: SourceRange) {
  case UnrecognisedToken(token: String, override val range: SourceRange) extends Diagnostic(s"Unrecognised token '$token'", range)
  
  case UnterminatedStringLiteral(override val range: SourceRange) extends Diagnostic(s"Unterminated string literal", range)
  
  case ExpectedSomething(expected: String, found: String, override val range: SourceRange) extends Diagnostic(s"Expected '$expected' but found '$found'", range)
  
  case SymbolAlreadyDeclared(name: String, override val range: SourceRange) extends Diagnostic(s"Symbol '$name' has already been declared in the current scope", range)
  
  case IllegalModifierCombination(modifier1: Modifier, modifier2: Modifier, override val range: SourceRange) extends Diagnostic(s"Illegal combination of modifiers '${modifier1.prettyPrint}' and '${modifier2.prettyPrint}'", range)
  
  case RepeatedModifier(modifier: Modifier, override val range: SourceRange) extends Diagnostic(s"Repeated modifier '${modifier.prettyPrint}'", range)
  
  case ModifierNotAllowed(modifier: Modifier, override val range: SourceRange) extends Diagnostic(s"Modifier '${modifier.prettyPrint}' not allowed here", range)
}