package parser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.*
import sources.SourceRange

class Parser {
  type ParserFunc[A] = ParserFunction[A, Token, Diagnostic]
  type SafePointFunc = SafePointFunction[Token]

  val parseModifierNode: Parsel[ModifierNode, Token, Diagnostic] = Parsel((input: Parsel.Input[Token]) =>
    input.current match {
      case Some(token) =>
        if (token.isModifier) {
          (Some(ModifierNode(token match {
            case Token.Static(range) => Modifier.STATIC
            case Token.Public(range) => Modifier.PUBLIC
            case Token.Private(range) => Modifier.PRIVATE
            case Token.Protected(range) => Modifier.PROTECTED
            case Token.Open(range) => Modifier.OPEN
            case Token.Final(range) => Modifier.FINAL
            case Token.Abstract(range) => Modifier.ABSTRACT
            case _ => throw IllegalStateException("Unreachable in parseModifierNode()")
          })), input.advance, Iterable.empty)
        } else {
          (None, input.advance, List(ExpectedSomething("modifier", token.prettyPrint, token.range)))
        }
      case None => (None, input.advance, List(ExpectedSomething("modifier", "EOF", input.last match {
        case Some(token) => token.range
        case None => SourceRange(-1, 0, 0)
      })))
    }
  )
}
