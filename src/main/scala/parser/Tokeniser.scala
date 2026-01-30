package parser

import diagnostics.Diagnostic
import Parsel.*
import ast.StringLiteral
import parser.Token.{CharLiteral, Identifier}
import sources.SourceRange

object Tokeniser {
  private def parseChar(char: Char): Parsel[Char, Char, Diagnostic] = {
    parseChar(_ == char)
  }

  private def parseChar(check: Char => Boolean): Parsel[Char, Char, Diagnostic] = {
    Parsel((input: Parsel.Input[Char]) => input.current match {
      case Some(c) if check(c) => (Some(c), input.advance, List.empty)
      case _ => (None, input, List.empty) // TODO
    })
  }

  private def parseString(str: String)(using fileId: Int): Parsel[String, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    assert(str.nonEmpty)
    parseCharsToT(str, _ => str)
  }

  private def parseWhitespaceChar: Parsel[Char, Char, Diagnostic] = {
    parseChar(_.isWhitespace)
  }

  // Parse single-line comment: // until newline
  private def parseSingleLineComment(using fileId: Int): Parsel[Unit, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    // Don't use parseString here - it would cause circular dependency with skipTrivia!
    (parseChar('/') ~> parseChar('/') ~> list(parseChar(_ != '\n')) <~ optional(parseChar('\n'))).map(_ => ())
  }

  // Parse multi-line comment: /* until */
  private def parseMultiLineComment: Parsel[Unit, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    // Parse /* then keep consuming until we find */
    def parseUntilCommentEnd: Parsel[Unit, Char, Diagnostic] = {
      Parsel((input: Input[Char]) => {
        var current = input
        var foundEnd = false

        while (!foundEnd && current.current.isDefined) {
          if (current.current.contains('*')) {
            val afterStar = current.advance
            if (afterStar.current.contains('/')) {
              current = afterStar.advance
              foundEnd = true
            } else {
              current = afterStar
            }
          } else {
            current = current.advance
          }
        }

        if (foundEnd) {
          (Some(()), current, List.empty)
        } else {
          // TODO: Unclosed comment error
//          (None, input, List.empty)
          (Some(()), current, List.empty)
        }
      })
    }

    parseChar('/') ~> parseChar('*') ~> parseUntilCommentEnd
  }

  // Skip any trivia (whitespace or comments)
  private def skipTrivia(using fileId: Int): Parsel[Unit, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    list(or(
      parseWhitespaceChar.map(_ => ()),
      parseSingleLineComment,
      parseMultiLineComment
    )).map(_ => ())
  }

  private def parseCharsToT[T](str: String, tokGen: SourceRange => T)(using fileId: Int)(using SafePointFunction[Char]): Parsel[T, Char, Diagnostic] = {
    assert(str.nonEmpty)
    var combinator = parseChar(str(0))
    str.tail.foldLeft(parseChar(str.head))((combinator, char) => combinator <~ parseChar(char)).map((_: Char, input: Input[Char]) => {
      tokGen(SourceRange(fileId, input.index, str.length))
    }) <~ skipTrivia
  }

  private val parseAlphabetOrUnderscore: Parsel[Char, Char, Diagnostic] = {
    parseChar(c => c.isLetter || c == '_')
  }

  private val parseAlphabetOrUnderscoreOrDigit: Parsel[Char, Char, Diagnostic] = {
    Parsel((input: Input[Char]) => input.current match {
      case Some(c) if c.isLetterOrDigit || c == '_' => (Some(c), input.advance, List.empty)
      case _ => (None, input, List.empty)
    })
  }

  private val parseDigit: Parsel[Char, Char, Diagnostic] = {
    Parsel((input: Input[Char]) => input.current match {
      case Some(c) if c.isDigit => (Some(c), input.advance, List.empty)
      case _ => (None, input, List.empty)
    })
  }
  // ===== Individual Token Parsers =====

  // 3-character operators (must come first for longest match)
  private def parseLeftShiftEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("<<=", Token.LeftShiftEquals(_))

  private def parseRightShiftEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(">>=", Token.RightShiftEquals(_))

  // 2-character operators
  private def parseDoubleEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("==", Token.DoubleEquals(_))

  private def parseNotEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("!=", Token.NotEquals(_))

  private def parseLessEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("<=", Token.LessEquals(_))

  private def parseGreaterEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(">=", Token.GreaterEquals(_))

  private def parseLeftShiftToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("<<", Token.LeftShift(_))

  private def parseRightShiftToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(">>", Token.RightShift(_))

  private def parsePlusEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("+=", Token.PlusEquals(_))

  private def parseMinusEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("-=", Token.MinusEquals(_))

  private def parseStarEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("*=", Token.StarEquals(_))

  private def parseSlashEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("/=", Token.SlashEquals(_))

  private def parseModuloEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("%=", Token.ModuloEquals(_))

  private def parseBitwiseAndEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("&=", Token.BitwiseAndEquals(_))

  private def parseBitwiseOrEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("|=", Token.BitwiseOrEquals(_))

  private def parseBitwiseXorEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("^=", Token.BitwiseXorEquals(_))

  private def parseIncrementToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("++", Token.Increment(_))

  private def parseDecrementToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("--", Token.Decrement(_))

  private def parseLogicalAndToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("&&", Token.LogicalAnd(_))

  private def parseLogicalOrToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("||", Token.LogicalOr(_))

  // Keywords (must come before identifier)
  private def parseProtectedToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("protected", Token.Protected(_))

  private def parseAbstractToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("abstract", Token.Abstract(_))

  private def parseInterfaceToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("interface", Token.Interface(_))

  private def parseContinueToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("continue", Token.Continue(_))

  private def parseOverrideToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("override", Token.Override(_))

  private def parseTypedefToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("typedef", Token.Typedef(_))

  private def parsePrivateToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("private", Token.Private(_))

  private def parseStaticToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("static", Token.Static(_))

  private def parsePublicToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("public", Token.Public(_))

  private def parseReturnToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("return", Token.Return(_))

  private def parseClassToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("class", Token.Class(_))

  private def parseFinalToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("final", Token.Final(_))

  private def parseWhileToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("while", Token.While(_))

  private def parseBreakToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("break", Token.Break(_))

  private def parseFalseToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("false", Token.False(_))

  private def parseOpenToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("open", Token.Open(_))

  private def parseTrueToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("true", Token.True(_))

  private def parseNullToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("null", Token.NullLiteral(_))

  private def parseElseToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("else", Token.Else(_))

  private def parseForToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("for", Token.For(_))

  private def parseOutToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("out", Token.Out(_))

  private def parseInToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("in", Token.In(_))

  private def parseIfToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("if", Token.If(_))

  // 1-character operators and punctuation
  private def parseColonToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(":", Token.Colon(_))

  private def parseSemiColonToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(";", Token.SemiColon(_))

  private def parseCommaToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(",", Token.Comma(_))

  private def parseLeftCurlyBraceToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("{", Token.LeftCurlyBrace(_))

  private def parseRightCurlyBraceToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("}", Token.RightCurlyBrace(_))

  private def parseLeftParenToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("(", Token.LeftParen(_))

  private def parseRightParenToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(")", Token.RightParen(_))

  private def parseLeftBracketToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("[", Token.LeftBracket(_))

  private def parseRightBracketToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("]", Token.RightBracket(_))

  private def parseEqualsToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("=", Token.Equals(_))

  private def parseLessToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("<", Token.Less(_))

  private def parseGreaterToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(">", Token.Greater(_))

  private def parseNotToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("!", Token.Not(_))

  private def parsePlusToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("+", Token.Plus(_))

  private def parseMinusToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("-", Token.Minus(_))

  private def parseStarToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("*", Token.Star(_))

  private def parseSlashToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("/", Token.Slash(_))

  private def parseModuloToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("%", Token.Modulo(_))

  private def parseBitwiseAndToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("&", Token.BitwiseAnd(_))

  private def parseBitwiseOrToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("|", Token.BitwiseOr(_))

  private def parseBitwiseXorToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("^", Token.BitwiseXor(_))

  private def parseQuestionToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT("?", Token.Question(_))

  private def parseDotToken(using fileId: Int)(using SafePointFunction[Char]): Parsel[Token, Char, Diagnostic] =
    parseCharsToT(".", Token.Dot(_))

  // Literals and identifiers
  private def parseStringLiteralToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    (parseChar('\"') ~> list(parseChar(_ != '"')) <~ parseChar('\"')).map((chars, input) =>
      Token.StringLiteral(chars.mkString, SourceRange(fileId, input.index, 2 + chars.length))
    ) <~ skipTrivia
  }

  private def parseCharLiteralToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    (parseChar('\'') ~> parseChar(_ != '\'') <~ parseChar('\'')).map((c, input) =>
      CharLiteral(c, SourceRange(fileId, input.index, 3))
    ) <~ skipTrivia // TODO - Change to 2 + length of char which could be 2 for '\n'
  }

  private def parseFloatLiteralToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    // Require at least one digit before the dot
    ((parseDigit ~ list(parseDigit) <~ parseChar('.')) ~ list(parseDigit)).map((tuple, input) => {
      val ((firstDigit, restDigits1), restDigits2) = tuple
      val allDigits1 = firstDigit :: restDigits1
      Token.FloatLiteral(s"${allDigits1.mkString}.${restDigits2.mkString}".toFloat, SourceRange(fileId, input.index, 1 + allDigits1.length + restDigits2.length))
    }) <~ skipTrivia
  }

  private def parseIntegerLiteralToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    // Require at least one digit
    (parseDigit ~ list(parseDigit)).map((tuple, input) => {
      val (firstDigit, restDigits) = tuple
      val allDigits = firstDigit :: restDigits
      Token.IntegerLiteral(allDigits.mkString.toInt, SourceRange(fileId, input.index, allDigits.length))
    }) <~ skipTrivia
  }

  private def parseIdentifierToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    (parseAlphabetOrUnderscore ~ list(parseAlphabetOrUnderscoreOrDigit)).map((tuple: (Char, List[Char]), input: Input[Char]) => {
      val (c, cs) = tuple
      Identifier((c :: cs).mkString, SourceRange(fileId, input.index, cs.size + 1))
    }) <~ skipTrivia
  }

  // ===== Main Token Parser =====

  private def parseToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    skipTrivia ~> or(
      parseLeftShiftEqualsToken,
      parseRightShiftEqualsToken,

      parseDoubleEqualsToken,
      parseNotEqualsToken,
      parseLessEqualsToken,
      parseGreaterEqualsToken,
      parseLeftShiftToken,
      parseRightShiftToken,
      parsePlusEqualsToken,
      parseMinusEqualsToken,
      parseStarEqualsToken,
      parseSlashEqualsToken,
      parseModuloEqualsToken,
      parseBitwiseAndEqualsToken,
      parseBitwiseOrEqualsToken,
      parseBitwiseXorEqualsToken,
      parseIncrementToken,
      parseDecrementToken,
      parseLogicalAndToken,
      parseLogicalOrToken,

      parseProtectedToken,
      parseAbstractToken,
      parseInterfaceToken,
      parseContinueToken,
      parseOverrideToken,
      parseTypedefToken,
      parsePrivateToken,
      parseStaticToken,
      parsePublicToken,
      parseReturnToken,
      parseClassToken,
      parseFinalToken,
      parseWhileToken,
      parseBreakToken,
      parseFalseToken,
      parseOpenToken,
      parseTrueToken,
      parseNullToken,
      parseElseToken,
      parseForToken,
      parseOutToken,
      parseInToken,
      parseIfToken,

      // Literals (float before integer for longest match)
      parseStringLiteralToken,
      parseCharLiteralToken,
      parseFloatLiteralToken,
      parseIntegerLiteralToken,

      parseIdentifierToken,

      parseColonToken,
      parseSemiColonToken,
      parseCommaToken,
      parseLeftCurlyBraceToken,
      parseRightCurlyBraceToken,
      parseLeftParenToken,
      parseRightParenToken,
      parseLeftBracketToken,
      parseRightBracketToken,
      parseEqualsToken,
      parseLessToken,
      parseGreaterToken,
      parseNotToken,
      parsePlusToken,
      parseMinusToken,
      parseStarToken,
      parseSlashToken,
      parseModuloToken,
      parseBitwiseAndToken,
      parseBitwiseOrToken,
      parseBitwiseXorToken,
      parseQuestionToken,
      parseDotToken
    )
  }

  // ===== Tokenize entire input =====

  private def tokeniser(using fileId: Int): Parsel[List[Token], Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    skipTrivia ~> list(parseToken)
  }
  
  def tokenise(string: String, fileId: Int): (Input[Token], List[Diagnostic]) = {
    given Int = fileId
    given SafePointFunction[Char] = _ => false
    val (optionalTokens, _, diagnostics) = tokeniser(Input(string.toCharArray.toVector))
    optionalTokens match {
      case Some(tokens) => (Input(tokens.toVector), diagnostics.toList)
      case None => (Input(Vector.empty), diagnostics.toList)
    }
  }
}
