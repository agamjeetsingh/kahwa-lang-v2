package parser

import diagnostics.Diagnostic
import Parsel.*
import ast.StringLiteral
import diagnostics.Diagnostic.UnrecognisedToken
import parser.Token.*
import sources.SourceRange

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  private def parseIdentifierOrKeywordToken(using fileId: Int): Parsel[Token, Char, Diagnostic] = {
    given SafePointFunction[Char] = _ => false
    (parseAlphabetOrUnderscore ~ list(parseAlphabetOrUnderscoreOrDigit)).map((tuple: (Char, List[Char]), input: Input[Char]) => {
      val (c, cs) = tuple
      val str = (c :: cs).mkString
      val sourceRange = SourceRange(fileId, input.index, cs.size + 1)
      str match
        case "protected" => Protected(sourceRange)
        case "abstract" => Abstract(sourceRange)
        case "interface" => Interface(sourceRange)
        case "continue" => Continue(sourceRange)
        case "override" => Override(sourceRange)
        case "typedef" => Typedef(sourceRange)
        case "private" => Private(sourceRange)
        case "static" => Static(sourceRange)
        case "public" => Public(sourceRange)
        case "return" => Return(sourceRange)
        case "class" => Class(sourceRange)
        case "final" => Final(sourceRange)
        case "while" => While(sourceRange)
        case "break" => Break(sourceRange)
        case "false" => False(sourceRange)
        case "open" => Open(sourceRange)
        case "true" => True(sourceRange)
        case "null" => NullLiteral(sourceRange)
        case "else" => Else(sourceRange)
        case "for" => For(sourceRange)
        case "out" => Out(sourceRange)
        case "in" => In(sourceRange)
        case "if" => If(sourceRange)
        case _ => Identifier(str, sourceRange)
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

      // Literals (float before integer for longest match)
      parseStringLiteralToken,
      parseCharLiteralToken,
      parseFloatLiteralToken,
      parseIntegerLiteralToken,

      parseIdentifierOrKeywordToken,

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
  
  def tokeniseSlow(string: String, fileId: Int): (Input[Token], List[Diagnostic]) = {
    given Int = fileId
    given SafePointFunction[Char] = _ => false
    val (optionalTokens, _, diagnostics) = tokeniser(Input(string.toCharArray.toVector))
    optionalTokens match {
      case Some(tokens) => (Input(tokens.toVector), diagnostics.toList)
      case None => (Input(Vector.empty), diagnostics.toList)
    }
  }

  case class TokeniserConfig(delimiters: List[Char],
                             exactTokens: Map[String, SourceRange => Token],
                             keywords: Map[String, SourceRange => Token]) {
    // Preprocess for efficient matching
    private[Tokeniser] lazy val delimiterSet: Set[Char] = delimiters.toSet
    private[Tokeniser] lazy val exactTokensByLength: TreeMap[Int, Map[String, SourceRange => Token]] =
      TreeMap.from(exactTokens.groupBy(_._1.length))(using Ordering[Int].reverse)
  }

  val config = TokeniserConfig(
    List(' ', '\t', '\r', '\n', '\f'),
    Map(
      "<<=" -> Token.LeftShiftEquals.apply,
      ">>=" -> Token.RightShiftEquals.apply,
      "==" -> Token.DoubleEquals.apply,
      "!=" -> Token.NotEquals.apply,
      "<=" -> Token.LessEquals.apply,
      ">=" -> Token.GreaterEquals.apply,
      "<<" -> Token.LeftShift.apply,
      ">>" -> Token.RightShift.apply,
      "+=" -> Token.PlusEquals.apply,
      "-=" -> Token.MinusEquals.apply,
      "*=" -> Token.StarEquals.apply,
      "/=" -> Token.SlashEquals.apply,
      "%=" -> Token.ModuloEquals.apply,
      "&=" -> Token.BitwiseAndEquals.apply,
      "|=" -> Token.BitwiseOrEquals.apply,
      "^=" -> Token.BitwiseXorEquals.apply,
      "++" -> Token.Increment.apply,
      "--" -> Token.Decrement.apply,
      "&&" -> Token.LogicalAnd.apply,
      "||" -> Token.LogicalOr.apply,
      ":" -> Token.Colon.apply,
      ";" -> Token.SemiColon.apply,
      "," -> Token.Comma.apply,
      "{" -> Token.LeftCurlyBrace.apply,
      "}" -> Token.RightCurlyBrace.apply,
      "(" -> Token.LeftParen.apply,
      ")" -> Token.RightParen.apply,
      "[" -> Token.LeftBracket.apply,
      "]" -> Token.RightBracket.apply,
      "=" -> Token.Equals.apply,
      "<" -> Token.Less.apply,
      ">" -> Token.Greater.apply,
      "!" -> Token.Not.apply,
      "+" -> Token.Plus.apply,
      "-" -> Token.Minus.apply,
      "*" -> Token.Star.apply,
      "/" -> Token.Slash.apply,
      "%" -> Token.Modulo.apply,
      "&" -> Token.BitwiseAnd.apply,
      "|" -> Token.BitwiseOr.apply,
      "^" -> Token.BitwiseXor.apply,
      "?" -> Token.Question.apply,
      "." -> Token.Dot.apply
    ),
    Map(
      "class" -> Token.Class.apply,
      "interface" -> Token.Interface.apply,
      "typedef" -> Token.Typedef.apply,
      "static" -> Token.Static.apply,
      "public" -> Token.Public.apply,
      "private" -> Token.Private.apply,
      "protected" -> Token.Protected.apply,
      "open" -> Token.Open.apply,
      "final" -> Token.Final.apply,
      "abstract" -> Token.Abstract.apply,
      "override" -> Token.Override.apply,
      "in" -> Token.In.apply,
      "out" -> Token.Out.apply,
      "return" -> Token.Return.apply,
      "if" -> Token.If.apply,
      "else" -> Token.Else.apply,
      "for" -> Token.For.apply,
      "while" -> Token.While.apply,
      "break" -> Token.Break.apply,
      "continue" -> Token.Continue.apply,
      "true" -> Token.True.apply,
      "false" -> Token.False.apply,
      "null" -> Token.NullLiteral.apply
    )
  )

  def tokenise(string: String, fileId: Int, tokeniserConfig: TokeniserConfig = config): (Input[Token], List[Diagnostic]) = {
    var idx = 0

    val exactTokens = tokeniserConfig.exactTokensByLength
    val tokens: mutable.ArrayBuffer[Token] = mutable.ArrayBuffer()
    val diagnostics: ListBuffer[Diagnostic] = mutable.ListBuffer()

    while (idx < string.length) {
      val startIdx = idx

      // Skip delimiters
      if (tokeniserConfig.delimiterSet.contains(string(idx))) {
        idx += 1
      } else {
        // Try to match exact tokens (longest first)
        var matched = false
        val iter = exactTokens.iterator
        while (iter.hasNext && !matched) {
          val (length, tokCandidates) = iter.next()
          if (startIdx + length <= string.length) {
            tokCandidates.get(string.slice(startIdx, startIdx + length)) match {
              case Some(rangeToToken) =>
                tokens += rangeToToken(SourceRange(fileId, startIdx, length))
                idx = startIdx + length
                matched = true
              case None =>
            }
          }
        }

        // If no exact token matched, try other token types
        if (!matched) {
          string(startIdx) match {
            case '\"' =>
              // Parse string literal
              idx = startIdx + 1
              val sb = new StringBuilder
              while (idx < string.length && string(idx) != '\"') {
                sb.append(string(idx))
                idx += 1
              }
              if (idx < string.length) {
                idx += 1 // Skip closing quote
                tokens += Token.StringLiteral(sb.toString, SourceRange(fileId, startIdx, idx - startIdx))
              } else {
                // Unclosed string literal - still add it with what we have
                tokens += Token.StringLiteral(sb.toString, SourceRange(fileId, startIdx, idx - startIdx))
              }

            case '\'' =>
              // Parse char literal
              idx = startIdx + 1
              if (idx < string.length) {
                val ch = string(idx)
                idx += 1
                if (idx < string.length && string(idx) == '\'') {
                  idx += 1
                  tokens += Token.CharLiteral(ch, SourceRange(fileId, startIdx, idx - startIdx))
                } else {
                  // Unclosed char literal
                  tokens += Token.CharLiteral(ch, SourceRange(fileId, startIdx, idx - startIdx))
                }
              } else {
                // Empty char literal
                diagnostics += UnrecognisedToken("'", SourceRange(fileId, startIdx, 1))
              }

            case c if c.isDigit =>
              // Parse number literal
              while (idx < string.length && string(idx).isDigit) {
                idx += 1
              }
              // Check for float (digit after dot required)
              if (idx < string.length && string(idx) == '.' && idx + 1 < string.length && string(idx + 1).isDigit) {
                idx += 1 // Skip dot
                while (idx < string.length && string(idx).isDigit) {
                  idx += 1
                }
                val value = string.slice(startIdx, idx).toFloat
                tokens += Token.FloatLiteral(value, SourceRange(fileId, startIdx, idx - startIdx))
              } else {
                val value = string.slice(startIdx, idx).toInt
                tokens += Token.IntegerLiteral(value, SourceRange(fileId, startIdx, idx - startIdx))
              }

            case c if c.isLetter || c == '_' =>
              // Parse identifier or keyword
              while (idx < string.length && (string(idx).isLetterOrDigit || string(idx) == '_')) {
                idx += 1
              }
              val text = string.slice(startIdx, idx)
              val token = tokeniserConfig.keywords.get(text) match {
                case Some(keywordConstructor) => keywordConstructor(SourceRange(fileId, startIdx, idx - startIdx))
                case None => Token.Identifier(text, SourceRange(fileId, startIdx, idx - startIdx))
              }
              tokens += token

            case c =>
              diagnostics += UnrecognisedToken(c.toString, SourceRange(fileId, startIdx, 1))
              idx += 1
          }
        }
      }
    }

    (Input(tokens.toVector), diagnostics.toList)
  }
}
