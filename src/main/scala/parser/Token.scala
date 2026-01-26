package parser

import ast.PrettyPrintable
import sources.SourceRange


enum Token extends PrettyPrintable {
  // Delimiters
  val range: SourceRange
  case Colon(range: SourceRange) extends Token

  case SemiColon(range: SourceRange) extends Token

  case Comma(range: SourceRange) extends Token

  case LeftCurlyBrace(range: SourceRange) extends Token

  case RightCurlyBrace(range: SourceRange) extends Token

  case LeftParen(range: SourceRange) extends Token

  case RightParen(range: SourceRange) extends Token

  case LeftBracket(range: SourceRange) extends Token

  case RightBracket(range: SourceRange) extends Token

  // Operators
  case Equals(range: SourceRange) extends Token

  case DoubleEquals(range: SourceRange) extends Token

  case Less(range: SourceRange) extends Token

  case Greater(range: SourceRange) extends Token

  case LessEquals(range: SourceRange) extends Token

  case GreaterEquals(range: SourceRange) extends Token

  case Not(range: SourceRange) extends Token

  case NotEquals(range: SourceRange) extends Token

  case Plus(range: SourceRange) extends Token

  case Minus(range: SourceRange) extends Token

  case Star(range: SourceRange) extends Token

  case Slash(range: SourceRange) extends Token

  case Modulo(range: SourceRange) extends Token

  case PlusEquals(range: SourceRange) extends Token

  case MinusEquals(range: SourceRange) extends Token

  case StarEquals(range: SourceRange) extends Token

  case SlashEquals(range: SourceRange) extends Token

  case ModuloEquals(range: SourceRange) extends Token

  case LeftShiftEquals(range: SourceRange) extends Token

  case RightShiftEquals(range: SourceRange) extends Token

  case BitwiseAndEquals(range: SourceRange) extends Token

  case BitwiseOrEquals(range: SourceRange) extends Token

  case BitwiseXorEquals(range: SourceRange) extends Token

  case Increment(range: SourceRange) extends Token

  case Decrement(range: SourceRange) extends Token

  case LogicalAnd(range: SourceRange) extends Token

  case LogicalOr(range: SourceRange) extends Token

  case BitwiseAnd(range: SourceRange) extends Token

  case BitwiseOr(range: SourceRange) extends Token

  case BitwiseXor(range: SourceRange) extends Token

  case LeftShift(range: SourceRange) extends Token

  case RightShift(range: SourceRange) extends Token

  case Question(range: SourceRange) extends Token

  case Dot(range: SourceRange) extends Token

  // Keywords
  case Class(range: SourceRange) extends Token

  case Static(range: SourceRange) extends Token

  case Public(range: SourceRange) extends Token

  case Private(range: SourceRange) extends Token

  case Protected(range: SourceRange) extends Token

  case Open(range: SourceRange) extends Token

  case Final(range: SourceRange) extends Token

  case Abstract(range: SourceRange) extends Token

  case Interface(range: SourceRange) extends Token

  case Typedef(range: SourceRange) extends Token

  case In(range: SourceRange) extends Token

  case Out(range: SourceRange) extends Token

  case Override(range: SourceRange) extends Token

  // Control flow
  case Return(range: SourceRange) extends Token

  case If(range: SourceRange) extends Token

  case Else(range: SourceRange) extends Token

  case For(range: SourceRange) extends Token

  case While(range: SourceRange) extends Token

  case Break(range: SourceRange) extends Token

  case Continue(range: SourceRange) extends Token

  // Literals - keywords
  case True(range: SourceRange) extends Token

  case False(range: SourceRange) extends Token

  case NullLiteral(range: SourceRange) extends Token

  // Tokens with values
  case Identifier(value: String, range: SourceRange) extends Token

  case StringLiteral(value: String, range: SourceRange) extends Token

  case CharLiteral(value: Char, range: SourceRange) extends Token

  case Integer(value: Int, range: SourceRange) extends Token

  case Float(value: Float, range: SourceRange) extends Token
  
  def isModifier: Boolean = this match {
    case _: Token.Static | Token.Public | Token.Private | Token.Protected |
            Token.Open | Token.Final | Token.Abstract => true
    case _ => false
  }
  
  override def prettyPrint: String = this match {
    case Token.Colon(_) => ":"
    case Token.SemiColon(_) => ";"
    case Token.Comma(_) => ","
    case Token.LeftCurlyBrace(_) => "{"
    case Token.RightCurlyBrace(_) => "}"
    case Token.LeftParen(_) => "("
    case Token.RightParen(_) => ")"
    case Token.LeftBracket(_) => "["
    case Token.RightBracket(_) => "]"
    case Token.Equals(_) => "="
    case Token.DoubleEquals(_) => "=="
    case Token.Less(_) => "<"
    case Token.Greater(_) => ">"
    case Token.LessEquals(_) => "<="
    case Token.GreaterEquals(_) => ">="
    case Token.Not(_) => "!"
    case Token.NotEquals(_) => "!="
    case Token.Plus(_) => "+"
    case Token.Minus(_) => "-"
    case Token.Star(_) => "*"
    case Token.Slash(_) => "/"
    case Token.Modulo(_) => "%"
    case Token.PlusEquals(_) => "+="
    case Token.MinusEquals(_) => "-="
    case Token.StarEquals(_) => "*="
    case Token.SlashEquals(_) => "/="
    case Token.ModuloEquals(_) => "%="
    case Token.LeftShiftEquals(_) => "<<="
    case Token.RightShiftEquals(_) => ">>="
    case Token.BitwiseAndEquals(_) => "&="
    case Token.BitwiseOrEquals(_) => "|="
    case Token.BitwiseXorEquals(_) => "^="
    case Token.Increment(_) => "++"
    case Token.Decrement(_) => "--"
    case Token.LogicalAnd(_) => "&&"
    case Token.LogicalOr(_) => "||"
    case Token.BitwiseAnd(_) => "&"
    case Token.BitwiseOr(_) => "|"
    case Token.BitwiseXor(_) => "^"
    case Token.LeftShift(_) => "<<"
    case Token.RightShift(_) => ">>"
    case Token.Question(_) => "?"
    case Token.Dot(_) => "."
    case Token.Class(_) => "class"
    case Token.Static(_) => "static"
    case Token.Public(_) => "public"
    case Token.Private(_) => "private"
    case Token.Protected(_) => "protected"
    case Token.Open(_) => "open"
    case Token.Final(_) => "final"
    case Token.Abstract(_) => "abstract"
    case Token.Interface(_) => "interface"
    case Token.Typedef(_) => "typedef"
    case Token.In(_) => "in"
    case Token.Out(_) => "out"
    case Token.Override(_) => "override"
    case Token.Return(_) => "return"
    case Token.If(_) => "if"
    case Token.Else(_) => "else"
    case Token.For(_) => "for"
    case Token.While(_) => "while"
    case Token.Break(_) => "break"
    case Token.Continue(_) => "continue"
    case Token.True(_) => "true"
    case Token.False(_) => "false"
    case Token.NullLiteral(_) => "null"
    case Token.Identifier(value, _) => value
    case Token.StringLiteral(value, _) => s""""$value""""
    case Token.CharLiteral(value, _) => value.toString
    case Token.Integer(value, _) => value.toString
    case Token.Float(value, _) => value.toString
  }
}