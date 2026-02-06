package parser

import diagnostics.Diagnostic
import Parsel.*
import diagnostics.Diagnostic.{UnrecognisedToken, UnterminatedStringLiteral}
import sources.SourceRange

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Tokeniser {
  case class TokeniserConfig(
      delimiters: List[Char],
      exactTokens: Map[String, SourceRange => Token],
      keywords: Map[String, SourceRange => Token]
  ) {
    // Preprocess for efficient matching
    private[Tokeniser] lazy val delimiterSet: Set[Char] = delimiters.toSet
    private[Tokeniser] lazy val exactTokensByLength: TreeMap[Int, Map[String, SourceRange => Token]] =
      TreeMap
        .from(exactTokens.groupBy(_._1.length))(using Ordering[Int].reverse)
  }

  private val config = TokeniserConfig(
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

  def tokenise(
      string: String,
      fileId: Int,
      tokeniserConfig: TokeniserConfig = config
  ): (Input[Token], List[Diagnostic]) = {
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
        // Check for comments first (before exact token matching)
        var isComment = false
        if (string(startIdx) == '/' && startIdx + 1 < string.length) {
          string(startIdx + 1) match {
            case '/' =>
              // Single-line comment: skip until newline or end of string
              idx = startIdx + 2
              while (idx < string.length && string(idx) != '\n') {
                idx += 1
              }
              isComment = true

            case '*' =>
              // Multi-line comment: skip until */ or end of string
              idx = startIdx + 2
              var foundEnd = false
              while (idx < string.length && !foundEnd) {
                if (
                  string(idx) == '*' && idx + 1 < string.length && string(
                    idx + 1
                  ) == '/'
                ) {
                  idx += 2
                  foundEnd = true
                } else {
                  idx += 1
                }
              }
              isComment = true

            case _ =>
            // Not a comment, continue to token matching
          }
        }

        if (!isComment) {
          // Try to match exact tokens (longest first)
          var matched = false
          val iter = exactTokens.iterator
          while (iter.hasNext && !matched) {
            val (length, tokCandidates) = iter.next()
            if (startIdx + length <= string.length) {
              tokCandidates.get(
                string.slice(startIdx, startIdx + length)
              ) match {
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
                  tokens += Token.StringLiteral(
                    sb.toString,
                    SourceRange(fileId, startIdx, idx - startIdx)
                  )
                } else {
                  // Unclosed string literal - still add it with what we have
                  tokens += Token.StringLiteral(
                    sb.toString,
                    SourceRange(fileId, startIdx, idx - startIdx)
                  )
                  diagnostics += UnterminatedStringLiteral(
                    SourceRange(fileId, startIdx)
                  )
                }

              case '\'' =>
                // Parse char literal
                idx = startIdx + 1
                if (idx < string.length) {
                  val ch = string(idx)
                  idx += 1
                  if (idx < string.length && string(idx) == '\'') {
                    idx += 1
                    tokens += Token.CharLiteral(
                      ch,
                      SourceRange(fileId, startIdx, idx - startIdx)
                    )
                  } else {
                    // Unclosed char literal
                    tokens += Token.CharLiteral(
                      ch,
                      SourceRange(fileId, startIdx, idx - startIdx)
                    )
                  }
                } else {
                  // Empty char literal
                  diagnostics += UnrecognisedToken(
                    "'",
                    SourceRange(fileId, startIdx, 1)
                  )
                }

              case c if c.isDigit =>
                // Parse number literal
                while (idx < string.length && string(idx).isDigit) {
                  idx += 1
                }
                // Check for float (digit after dot required)
                if (
                  idx < string.length && string(
                    idx
                  ) == '.' && idx + 1 < string.length && string(idx + 1).isDigit
                ) {
                  idx += 1 // Skip dot
                  while (idx < string.length && string(idx).isDigit) {
                    idx += 1
                  }
                  val value: Float = string.slice(startIdx, idx).toFloat
                  tokens += Token.FloatLiteral(
                    value,
                    SourceRange(fileId, startIdx, idx - startIdx)
                  )
                } else {
                  val value: Int = string.slice(startIdx, idx).toInt
                  tokens += Token.IntegerLiteral(
                    value,
                    SourceRange(fileId, startIdx, idx - startIdx)
                  )
                }

              case c if c.isLetter || c == '_' =>
                // Parse identifier or keyword
                while (
                  idx < string.length && (string(idx).isLetterOrDigit || string(
                    idx
                  ) == '_')
                ) {
                  idx += 1
                }
                val text = string.slice(startIdx, idx)
                tokens += (tokeniserConfig.keywords.get(text) match {
                  case Some(keywordConstructor) =>
                    keywordConstructor(
                      SourceRange(fileId, startIdx, idx - startIdx)
                    )
                  case None =>
                    Token.Identifier(
                      text,
                      SourceRange(fileId, startIdx, idx - startIdx)
                    )
                })

              case c =>
                diagnostics += UnrecognisedToken(
                  c.toString,
                  SourceRange(fileId, startIdx)
                )
                idx += 1
            }
          }
        }
      }
    }

    (Input(tokens.toVector), diagnostics.toList)
  }
}
