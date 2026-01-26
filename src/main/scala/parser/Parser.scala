package parser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.*
import sources.SourceRange

class Parser {
  type ParserFunc[A] = ParserFunction[A, Token, Diagnostic]
  type SafePointFunc = SafePointFunction[Token]
  
  val parseModifierNode: Parsel[ModifierNode, Token, Diagnostic] = parseToken(token =>
    if (token.isModifier) {
      Some(ModifierNode(token match {
        case Token.Static(_) => Modifier.STATIC
        case Token.Public(_) => Modifier.PUBLIC
        case Token.Private(_) => Modifier.PRIVATE
        case Token.Protected(_) => Modifier.PROTECTED
        case Token.Open(_) => Modifier.OPEN
        case Token.Final(_) => Modifier.FINAL
        case Token.Abstract(_) => Modifier.ABSTRACT
        case _ => throw IllegalStateException("Unreachable in parseModifierNode()")
      }))
    } else {
      None
    }, token => ExpectedSomething("modifier", token.prettyPrint, token.range), range => ExpectedSomething("modifier", "EOF", range))
  
  def parseToken[A](tokenMatch: Token => Option[A], notMatchError: Token => Diagnostic, endOfFileError: SourceRange => Diagnostic): Parsel[A, Token, Diagnostic] = Parsel((input: Parsel.Input[Token]) =>
    input.current match {
      case Some(token) => {
        val matched = tokenMatch(token)
        matched match {
          case Some(value: A) => (matched, input.advance, Iterable.empty)
          case None => (None, input.advance, List(notMatchError(token)))
        }
      }
      case None => (None, input.advance, List(endOfFileError(input.last match {
        case Some(token) => token.range
        case None => SourceRange(-1, 0, 0)
      })))
    })
  
  def parseTok[A](expected: String)(check: PartialFunction[Token, A]): Parsel[A, Token, Diagnostic] =
    parseToken(
      check.lift,
      token => ExpectedSomething(expected, token.prettyPrint, token.range),
      range => ExpectedSomething(expected, "EOF", range)
    )

  // Pre-defined token parsers
  private val parseColon = parseTok(":") { case tok: Token.Colon => tok }
  private val parseSemiColon = parseTok(";") { case tok: Token.SemiColon => tok }
  private val parseComma = parseTok(",") { case tok: Token.Comma => tok }
  private val parseLeftCurlyBrace = parseTok("{") { case tok: Token.LeftCurlyBrace => tok }
  private val parseRightCurlyBrace = parseTok("}") { case tok: Token.RightCurlyBrace => tok }
  private val parseLeftParen = parseTok("(") { case tok: Token.LeftParen => tok }
  private val parseRightParen = parseTok(")") { case tok: Token.RightParen => tok }
  private val parseLeftBracket = parseTok("[") { case tok: Token.LeftBracket => tok }
  private val parseRightBracket = parseTok("]") { case tok: Token.RightBracket => tok }

  private val parseEquals = parseTok("=") { case tok: Token.Equals => tok }
  private val parseDoubleEquals = parseTok("==") { case tok: Token.DoubleEquals => tok }
  private val parseLess = parseTok("<") { case tok: Token.Less => tok }
  private val parseGreater = parseTok(">") { case tok: Token.Greater => tok }
  private val parseLessEquals = parseTok("<=") { case tok: Token.LessEquals => tok }
  private val parseGreaterEquals = parseTok(">=") { case tok: Token.GreaterEquals => tok }
  private val parseNot = parseTok("!") { case tok: Token.Not => tok }
  private val parseNotEquals = parseTok("!=") { case tok: Token.NotEquals => tok }
  private val parsePlus = parseTok("+") { case tok: Token.Plus => tok }
  private val parseMinus = parseTok("-") { case tok: Token.Minus => tok }
  private val parseStar = parseTok("*") { case tok: Token.Star => tok }
  private val parseSlash = parseTok("/") { case tok: Token.Slash => tok }
  private val parseModulo = parseTok("%") { case tok: Token.Modulo => tok }
  private val parsePlusEquals = parseTok("+=") { case tok: Token.PlusEquals => tok }
  private val parseMinusEquals = parseTok("-=") { case tok: Token.MinusEquals => tok }
  private val parseStarEquals = parseTok("*=") { case tok: Token.StarEquals => tok }
  private val parseSlashEquals = parseTok("/=") { case tok: Token.SlashEquals => tok }
  private val parseModuloEquals = parseTok("%=") { case tok: Token.ModuloEquals => tok }
  private val parseLeftShiftEquals = parseTok("<<=") { case tok: Token.LeftShiftEquals => tok }
  private val parseRightShiftEquals = parseTok(">>=") { case tok: Token.RightShiftEquals => tok }
  private val parseBitwiseAndEquals = parseTok("&=") { case tok: Token.BitwiseAndEquals => tok }
  private val parseBitwiseOrEquals = parseTok("|=") { case tok: Token.BitwiseOrEquals => tok }
  private val parseBitwiseXorEquals = parseTok("^=") { case tok: Token.BitwiseXorEquals => tok }
  private val parseIncrement = parseTok("++") { case tok: Token.Increment => tok }
  private val parseDecrement = parseTok("--") { case tok: Token.Decrement => tok }
  private val parseLogicalAnd = parseTok("&&") { case tok: Token.LogicalAnd => tok }
  private val parseLogicalOr = parseTok("||") { case tok: Token.LogicalOr => tok }
  private val parseBitwiseAnd = parseTok("&") { case tok: Token.BitwiseAnd => tok }
  private val parseBitwiseOr = parseTok("|") { case tok: Token.BitwiseOr => tok }
  private val parseBitwiseXor = parseTok("^") { case tok: Token.BitwiseXor => tok }
  private val parseLeftShift = parseTok("<<") { case tok: Token.LeftShift => tok }
  private val parseRightShift = parseTok(">>") { case tok: Token.RightShift => tok }
  private val parseQuestion = parseTok("?") { case tok: Token.Question => tok }
  private val parseDot = parseTok(".") { case tok: Token.Dot => tok }

  private val parseClass = parseTok("class") { case tok: Token.Class => tok }
  private val parseStatic = parseTok("static") { case tok: Token.Static => tok }
  private val parsePublic = parseTok("public") { case tok: Token.Public => tok }
  private val parsePrivate = parseTok("private") { case tok: Token.Private => tok }
  private val parseProtected = parseTok("protected") { case tok: Token.Protected => tok }
  private val parseOpen = parseTok("open") { case tok: Token.Open => tok }
  private val parseFinal = parseTok("final") { case tok: Token.Final => tok }
  private val parseAbstract = parseTok("abstract") { case tok: Token.Abstract => tok }
  private val parseInterface = parseTok("interface") { case tok: Token.Interface => tok }
  private val parseTypedef = parseTok("typedef") { case tok: Token.Typedef => tok }
  private val parseIn = parseTok("in") { case tok: Token.In => tok }
  private val parseOut = parseTok("out") { case tok: Token.Out => tok }
  private val parseOverride = parseTok("override") { case tok: Token.Override => tok }

  private val parseReturn = parseTok("return") { case tok: Token.Return => tok }
  private val parseIf = parseTok("if") { case tok: Token.If => tok }
  private val parseElse = parseTok("else") { case tok: Token.Else => tok }
  private val parseFor = parseTok("for") { case tok: Token.For => tok }
  private val parseWhile = parseTok("while") { case tok: Token.While => tok }
  private val parseBreak = parseTok("break") { case tok: Token.Break => tok }
  private val parseContinue = parseTok("continue") { case tok: Token.Continue => tok }

  private val parseTrue = parseTok("true") { case tok: Token.True => tok }
  private val parseFalse = parseTok("false") { case tok: Token.False => tok }
  private val parseNull = parseTok("null") { case tok: Token.NullLiteral => tok }

  private val parseIdentifier = parseTok("identifier") { case tok: Token.Identifier => tok }
  private val parseStringLiteral = parseTok("string literal") { case tok: Token.StringLiteral => tok }
  private val parseCharLiteral = parseTok("char literal") { case tok: Token.CharLiteral => tok }
  private val parseInteger = parseTok("integer") { case tok: Token.Integer => tok }
  private val parseFloat = parseTok("float") { case tok: Token.Float => tok }
}
