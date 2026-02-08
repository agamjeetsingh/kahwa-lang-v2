package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sources.SourceRange
import diagnostics.Diagnostic

class TokeniserTest extends AnyFlatSpec with Matchers {

  // Test token factories for comparison (ignoring source ranges)
  object TestTokens {
    val Colon = Token.Colon(SourceRange.dummy)
    val SemiColon = Token.SemiColon(SourceRange.dummy)
    val Comma = Token.Comma(SourceRange.dummy)
    val LeftCurlyBrace = Token.LeftCurlyBrace(SourceRange.dummy)
    val RightCurlyBrace = Token.RightCurlyBrace(SourceRange.dummy)
    val LeftParen = Token.LeftParen(SourceRange.dummy)
    val RightParen = Token.RightParen(SourceRange.dummy)
    val LeftBracket = Token.LeftBracket(SourceRange.dummy)
    val RightBracket = Token.RightBracket(SourceRange.dummy)
    val Equals = Token.Equals(SourceRange.dummy)
    val Less = Token.Less(SourceRange.dummy)
    val Greater = Token.Greater(SourceRange.dummy)
    val Not = Token.Not(SourceRange.dummy)
    val Plus = Token.Plus(SourceRange.dummy)
    val Minus = Token.Minus(SourceRange.dummy)
    val Star = Token.Star(SourceRange.dummy)
    val Slash = Token.Slash(SourceRange.dummy)
    val Modulo = Token.Modulo(SourceRange.dummy)
    val BitwiseOr = Token.BitwiseOr(SourceRange.dummy)
    val BitwiseXor = Token.BitwiseXor(SourceRange.dummy)
    val Question = Token.Question(SourceRange.dummy)
    val Dot = Token.Dot(SourceRange.dummy)
    val BitwiseAnd = Token.BitwiseAnd(SourceRange.dummy)

    val DoubleEquals = Token.DoubleEquals(SourceRange.dummy)
    val NotEquals = Token.NotEquals(SourceRange.dummy)
    val LessEquals = Token.LessEquals(SourceRange.dummy)
    val GreaterEquals = Token.GreaterEquals(SourceRange.dummy)
    val LeftShift = Token.LeftShift(SourceRange.dummy)
    val RightShift = Token.RightShift(SourceRange.dummy)
    val LeftShiftEquals = Token.LeftShiftEquals(SourceRange.dummy)
    val RightShiftEquals = Token.RightShiftEquals(SourceRange.dummy)
    val PlusEquals = Token.PlusEquals(SourceRange.dummy)
    val MinusEquals = Token.MinusEquals(SourceRange.dummy)
    val StarEquals = Token.StarEquals(SourceRange.dummy)
    val SlashEquals = Token.SlashEquals(SourceRange.dummy)
    val ModuloEquals = Token.ModuloEquals(SourceRange.dummy)
    val BitwiseAndEquals = Token.BitwiseAndEquals(SourceRange.dummy)
    val BitwiseOrEquals = Token.BitwiseOrEquals(SourceRange.dummy)
    val BitwiseXorEquals = Token.BitwiseXorEquals(SourceRange.dummy)
    val Increment = Token.Increment(SourceRange.dummy)
    val Decrement = Token.Decrement(SourceRange.dummy)
    val LogicalAnd = Token.LogicalAnd(SourceRange.dummy)
    val LogicalOr = Token.LogicalOr(SourceRange.dummy)

    val Class = Token.Class(SourceRange.dummy)
    val Interface = Token.Interface(SourceRange.dummy)
    val If = Token.If(SourceRange.dummy)
    val Else = Token.Else(SourceRange.dummy)
    val For = Token.For(SourceRange.dummy)
    val While = Token.While(SourceRange.dummy)
    val Return = Token.Return(SourceRange.dummy)
    val True = Token.True(SourceRange.dummy)
    val False = Token.False(SourceRange.dummy)
    val Public = Token.Public(SourceRange.dummy)
    val Private = Token.Private(SourceRange.dummy)
    val Protected = Token.Protected(SourceRange.dummy)
    val Final = Token.Final(SourceRange.dummy)
    val Open = Token.Open(SourceRange.dummy)
    val Abstract = Token.Abstract(SourceRange.dummy)
    val Break = Token.Break(SourceRange.dummy)
    val Continue = Token.Continue(SourceRange.dummy)
    val Typedef = Token.Typedef(SourceRange.dummy)
    val Override = Token.Override(SourceRange.dummy)

    def Identifier(value: String) = Token.Identifier(value, SourceRange.dummy)
    def StringLiteral(value: String) =
      Token.StringLiteral(value, SourceRange.dummy)
    def CharLiteral(value: Char) = Token.CharLiteral(value, SourceRange.dummy)
    def IntegerLiteral(value: Int) =
      Token.IntegerLiteral(value, SourceRange.dummy)
    def FloatLiteral(value: Float) =
      Token.FloatLiteral(value, SourceRange.dummy)
  }

  import TestTokens._

  val singleLengthTokens: List[(String, Token)] = List(
    (":", Colon),
    (";", SemiColon),
    (",", Comma),
    ("{", LeftCurlyBrace),
    ("}", RightCurlyBrace),
    ("(", LeftParen),
    (")", RightParen),
    ("[", LeftBracket),
    ("]", RightBracket),
    ("=", Equals),
    ("<", Less),
    (">", Greater),
    ("!", Not),
    ("+", Plus),
    ("-", Minus),
    ("*", Star),
    ("/", Slash),
    ("%", Modulo),
    ("|", BitwiseOr),
    ("^", BitwiseXor),
    ("?", Question),
    (".", Dot)
  )

  val keywordTokens: List[(String, Token)] = List(
    ("class", Class),
    ("interface", Interface),
    ("if", If),
    ("else", Else),
    ("for", For),
    ("while", While),
    ("return", Return),
    ("true", True),
    ("false", False),
    ("public", Public),
    ("private", Private),
    ("protected", Protected),
    ("final", Final),
    ("open", Open),
    ("abstract", Abstract),
    ("break", Break),
    ("continue", Continue),
    ("typedef", Typedef),
    ("override", Override)
  )

  // Helper to check if two tokens match (ignoring source range)
  def tokenMatches(actual: Token, expected: Token): Boolean =
    (actual, expected) match {
      case (_: Token.Colon, _: Token.Colon) => true
      case (_: Token.SemiColon, _: Token.SemiColon) => true
      case (_: Token.Comma, _: Token.Comma) => true
      case (_: Token.LeftCurlyBrace, _: Token.LeftCurlyBrace) => true
      case (_: Token.RightCurlyBrace, _: Token.RightCurlyBrace) => true
      case (_: Token.LeftParen, _: Token.LeftParen) => true
      case (_: Token.RightParen, _: Token.RightParen) => true
      case (_: Token.LeftBracket, _: Token.LeftBracket) => true
      case (_: Token.RightBracket, _: Token.RightBracket) => true
      case (_: Token.Equals, _: Token.Equals) => true
      case (_: Token.DoubleEquals, _: Token.DoubleEquals) => true
      case (_: Token.Less, _: Token.Less) => true
      case (_: Token.Greater, _: Token.Greater) => true
      case (_: Token.LessEquals, _: Token.LessEquals) => true
      case (_: Token.GreaterEquals, _: Token.GreaterEquals) => true
      case (_: Token.Not, _: Token.Not) => true
      case (_: Token.NotEquals, _: Token.NotEquals) => true
      case (_: Token.Plus, _: Token.Plus) => true
      case (_: Token.Minus, _: Token.Minus) => true
      case (_: Token.Star, _: Token.Star) => true
      case (_: Token.Slash, _: Token.Slash) => true
      case (_: Token.Modulo, _: Token.Modulo) => true
      case (_: Token.PlusEquals, _: Token.PlusEquals) => true
      case (_: Token.MinusEquals, _: Token.MinusEquals) => true
      case (_: Token.StarEquals, _: Token.StarEquals) => true
      case (_: Token.SlashEquals, _: Token.SlashEquals) => true
      case (_: Token.ModuloEquals, _: Token.ModuloEquals) => true
      case (_: Token.LeftShiftEquals, _: Token.LeftShiftEquals) => true
      case (_: Token.RightShiftEquals, _: Token.RightShiftEquals) => true
      case (_: Token.BitwiseAndEquals, _: Token.BitwiseAndEquals) => true
      case (_: Token.BitwiseOrEquals, _: Token.BitwiseOrEquals) => true
      case (_: Token.BitwiseXorEquals, _: Token.BitwiseXorEquals) => true
      case (_: Token.Increment, _: Token.Increment) => true
      case (_: Token.Decrement, _: Token.Decrement) => true
      case (_: Token.LogicalAnd, _: Token.LogicalAnd) => true
      case (_: Token.LogicalOr, _: Token.LogicalOr) => true
      case (_: Token.BitwiseAnd, _: Token.BitwiseAnd) => true
      case (_: Token.BitwiseOr, _: Token.BitwiseOr) => true
      case (_: Token.BitwiseXor, _: Token.BitwiseXor) => true
      case (_: Token.LeftShift, _: Token.LeftShift) => true
      case (_: Token.RightShift, _: Token.RightShift) => true
      case (_: Token.Question, _: Token.Question) => true
      case (_: Token.Dot, _: Token.Dot) => true
      case (_: Token.Class, _: Token.Class) => true
      case (_: Token.Public, _: Token.Public) => true
      case (_: Token.Private, _: Token.Private) => true
      case (_: Token.Protected, _: Token.Protected) => true
      case (_: Token.Open, _: Token.Open) => true
      case (_: Token.Final, _: Token.Final) => true
      case (_: Token.Abstract, _: Token.Abstract) => true
      case (_: Token.Interface, _: Token.Interface) => true
      case (_: Token.Typedef, _: Token.Typedef) => true
      case (_: Token.Override, _: Token.Override) => true
      case (_: Token.Return, _: Token.Return) => true
      case (_: Token.If, _: Token.If) => true
      case (_: Token.Else, _: Token.Else) => true
      case (_: Token.For, _: Token.For) => true
      case (_: Token.While, _: Token.While) => true
      case (_: Token.Break, _: Token.Break) => true
      case (_: Token.Continue, _: Token.Continue) => true
      case (_: Token.True, _: Token.True) => true
      case (_: Token.False, _: Token.False) => true
      case (Token.Identifier(v1, _), Token.Identifier(v2, _)) => v1 == v2
      case (Token.StringLiteral(v1, _), Token.StringLiteral(v2, _)) => v1 == v2
      case (Token.CharLiteral(v1, _), Token.CharLiteral(v2, _)) => v1 == v2
      case (Token.IntegerLiteral(v1, _), Token.IntegerLiteral(v2, _)) =>
        v1 == v2
      case (Token.FloatLiteral(v1, _), Token.FloatLiteral(v2, _)) =>
        Math.abs(v1 - v2) < 1e-6f
      case _ => false
    }

  def expectTokenSequence(input: String, expected: Token*): Unit = {
    val (tokenInput, _) = Tokeniser.tokenise(input, 0)
    val tokens = tokenInput.source

    tokens should have size expected.size
    tokens.zip(expected).zipWithIndex.foreach { case ((actual, exp), idx) =>
      withClue(s"Token at index $idx: ") {
        tokenMatches(actual, exp) shouldBe true
      }
    }
  }

  def expectNoDiagnostics(input: String): Unit = {
    val (_, diagnostics) = Tokeniser.tokenise(input, 0)
    diagnostics shouldBe empty
  }

  def testEqualsOperatorSequences(
      baseOpEquals: Token,
      baseOpStr: String
  ): Unit = {
    expectTokenSequence(s"$baseOpStr=", baseOpEquals)
    expectTokenSequence(s"$baseOpStr==", baseOpEquals, Equals)
    expectTokenSequence(s"$baseOpStr===", baseOpEquals, DoubleEquals)
  }

  def unTokenise(tokens: Vector[Token]): String = {
    if (tokens.isEmpty) return ""
    var res = ""
    val fileId = tokens.head.range.fileId

    tokens.foreach { token =>
      res.length should be <= token.range.pos
      res = res + " " * (token.range.pos - res.length)

      val str = token match {
        case Token.FloatLiteral(value, range) =>
          value.toString.take(range.length)
        case _ => token.prettyPrint
      }

      str.length shouldBe token.range.length
      token.range.fileId shouldBe fileId
      res = res + str
    }
    res
  }

  // ===== Tests =====

  "Tokeniser" should "tokenise single length tokens correctly" in {
    singleLengthTokens.foreach { case (str, expectedToken) =>
      expectTokenSequence(str, expectedToken)
      expectNoDiagnostics(str)
    }
  }

  it should "tokenise multiple single length tokens correctly" in {
    singleLengthTokens.foreach { case (str1, tok1) =>
      singleLengthTokens.foreach { case (str2, tok2) =>
        singleLengthTokens.foreach { case (str3, tok3) =>
          singleLengthTokens.foreach { case (str4, tok4) =>
            val input = s"$str1 $str2\t$str3\n$str4"
            expectTokenSequence(input, tok1, tok2, tok3, tok4)
          }
        }
      }
    }
    expectNoDiagnostics(singleLengthTokens.map(_._1).mkString(" "))
  }

  it should "tokenise multiple length tokens with longest matching" in {
    expectTokenSequence("==", DoubleEquals)
    expectTokenSequence("===", DoubleEquals, Equals)
    expectTokenSequence("====", DoubleEquals, DoubleEquals)

    expectTokenSequence("<=", LessEquals)
    expectTokenSequence("<<=", LeftShiftEquals)
    expectTokenSequence("<<<=", LeftShift, LessEquals)
    expectTokenSequence("<==", LessEquals, Equals)
    expectTokenSequence("<===", LessEquals, DoubleEquals)
    expectTokenSequence("<<==", LeftShiftEquals, Equals)
    expectTokenSequence("<<===", LeftShiftEquals, DoubleEquals)

    expectTokenSequence(">=", GreaterEquals)
    expectTokenSequence(">>=", RightShiftEquals)
    expectTokenSequence(">>>=", RightShift, GreaterEquals)
    expectTokenSequence(">==", GreaterEquals, Equals)
    expectTokenSequence(">===", GreaterEquals, DoubleEquals)
    expectTokenSequence(">>==", RightShiftEquals, Equals)
    expectTokenSequence(">>===", RightShiftEquals, DoubleEquals)

    expectTokenSequence("!=", NotEquals)
    expectTokenSequence("!==", NotEquals, Equals)
    expectTokenSequence("!===", NotEquals, DoubleEquals)
    expectTokenSequence("!!===", Not, NotEquals, DoubleEquals)

    testEqualsOperatorSequences(PlusEquals, "+")
    testEqualsOperatorSequences(MinusEquals, "-")
    testEqualsOperatorSequences(StarEquals, "*")
    testEqualsOperatorSequences(SlashEquals, "/")
    testEqualsOperatorSequences(ModuloEquals, "%")
    testEqualsOperatorSequences(BitwiseAndEquals, "&")
    testEqualsOperatorSequences(BitwiseOrEquals, "|")
    testEqualsOperatorSequences(BitwiseXorEquals, "^")

    expectTokenSequence("++", Increment)
    expectTokenSequence("+++", Increment, Plus)

    expectTokenSequence("--", Decrement)
    expectTokenSequence("---", Decrement, Minus)

    expectTokenSequence("||", LogicalOr)
    expectTokenSequence("|||", LogicalOr, BitwiseOr)

    expectTokenSequence("&&", LogicalAnd)
    expectTokenSequence("&&&", LogicalAnd, BitwiseAnd)

    expectNoDiagnostics("==")
  }

  it should "tokenise integers correctly" in {
    (-1000 until 1000).foreach { i =>
      if (i < 0) {
        expectTokenSequence(i.toString, Minus, IntegerLiteral(-i))
      } else {
        expectTokenSequence(i.toString, IntegerLiteral(i))
      }

      val (tokenInput, _) = Tokeniser.tokenise(i.toString, 0)
      val tokens = tokenInput.source
      val numTok = tokens.last
      numTok match {
        case Token.IntegerLiteral(value, _) => value shouldBe Math.abs(i)
        case _ => fail(s"Expected IntegerLiteral but got $numTok")
      }
    }

    expectNoDiagnostics("42")
  }

  it should "tokenise floats correctly" in {
    val random = new scala.util.Random(42)

    (0 until 10000).foreach { _ =>
      val num = (random.nextDouble() * 200 - 100).toFloat
      if (num < 0) {
        expectTokenSequence(num.toString, Minus, FloatLiteral(-num))
      } else {
        expectTokenSequence(num.toString, FloatLiteral(num))
      }

      val (tokenInput, _) = Tokeniser.tokenise(num.toString, 0)
      val tokens = tokenInput.source
      val numTok = tokens.last
      numTok match {
        case Token.FloatLiteral(value, _) =>
          Math.abs(value - Math.abs(num)) should be < 1e-6f
        case _ => fail(s"Expected FloatLiteral but got $numTok")
      }
    }

    expectTokenSequence("1.0", FloatLiteral(1.0f))
    expectTokenSequence("1.", IntegerLiteral(1), Dot)
    expectTokenSequence(
      "1.a abc",
      IntegerLiteral(1),
      Dot,
      Identifier("a"),
      Identifier("abc")
    )

    expectNoDiagnostics("1.0")
  }

  it should "tokenise strings correctly" in {
    ('a' to 'z').foreach { i =>
      ('a' to 'z').foreach { j =>
        ('a' to 'z').foreach { k =>
          val str = s"\"$i$j$k\""
          val (tokenInput, _) = Tokeniser.tokenise(str, 0)
          val tokens = tokenInput.source

          tokens should have size 1
          tokens.head match {
            case Token.StringLiteral(value, _) => value shouldBe s"$i$j$k"
            case _ => fail(s"Expected StringLiteral but got ${tokens.head}")
          }
        }
      }
    }

    // Testing with spaces and other delimiters
    val delimiters = List(' ', '\t', '\r', '\n', '\f')
    delimiters.foreach { delimiter =>
      val str = s"\"abc123 $delimiter 123 Weird char Φ abc\""
      val (tokenInput, _) = Tokeniser.tokenise(str, 0)
      val tokens = tokenInput.source

      tokens should have size 1
      tokens.head match {
        case Token.StringLiteral(value, _) =>
          value shouldBe str.substring(1, str.length - 1)
        case _ => fail(s"Expected StringLiteral but got ${tokens.head}")
      }
    }

    expectNoDiagnostics("\"hello\"")
  }

  it should "tokenise multiple token types correctly" in {
    val allTokens = singleLengthTokens ++ List(
      ("==", DoubleEquals),
      ("!=", NotEquals),
      ("<=", LessEquals),
      (">=", GreaterEquals),
      ("<<", LeftShift),
      (">>", RightShift),
      ("+=", PlusEquals),
      ("-=", MinusEquals),
      ("*=", StarEquals),
      ("/=", SlashEquals),
      ("%=", ModuloEquals),
      ("&=", BitwiseAndEquals),
      ("|=", BitwiseOrEquals),
      ("^=", BitwiseXorEquals),
      ("<<=", LeftShiftEquals),
      (">>=", RightShiftEquals),
      ("++", Increment),
      ("--", Decrement),
      ("&&", LogicalAnd),
      ("||", LogicalOr),
      ("&", BitwiseAnd)
    ) ++ keywordTokens

    allTokens.foreach { case (str1, tok1) =>
      allTokens.foreach { case (str2, tok2) =>
        allTokens.foreach { case (str3, tok3) =>
          val input = s"$str1 \n$str2\t\r$str3"
          expectTokenSequence(input, tok1, tok2, tok3)
        }
      }
    }

    expectNoDiagnostics("class if for")
  }

  it should "tokenise identifiers correctly" in {
    expectTokenSequence(
      "abc123 234 abc _123 123_",
      Identifier("abc123"),
      IntegerLiteral(234),
      Identifier("abc"),
      Identifier("_123"),
      IntegerLiteral(123),
      Identifier("_")
    )

    expectTokenSequence(
      "class MyClass : SomeOtherClass {\n void foo() { } \n}",
      Class,
      Identifier("MyClass"),
      Colon,
      Identifier("SomeOtherClass"),
      LeftCurlyBrace,
      Identifier("void"),
      Identifier("foo"),
      LeftParen,
      RightParen,
      LeftCurlyBrace,
      RightCurlyBrace,
      RightCurlyBrace
    )

    expectNoDiagnostics("abc123")
  }

  it should "identify keywords separately from identifiers" in {
    val keywordStr = keywordTokens.map(_._1).mkString(" ")
    expectTokenSequence(keywordStr, keywordTokens.map(_._2): _*)

    expectTokenSequence(
      "class _class for for-who for() if",
      Class,
      Identifier("_class"),
      For,
      For,
      Minus,
      Identifier("who"),
      For,
      LeftParen,
      RightParen,
      If
    )

    expectNoDiagnostics("class if for")
  }

  it should "skip single line comments" in {
    expectTokenSequence("// This is a comment")

    expectTokenSequence(
      "// This is a comment \n tokenise normally",
      Identifier("tokenise"),
      Identifier("normally")
    )

    expectTokenSequence(
      "// This is a comment \t This too is part of the comment \n" +
        "123456 // Comment can begin after something too",
      IntegerLiteral(123456)
    )

    expectTokenSequence(
      "/ / This is not a comment",
      Slash,
      Slash,
      Identifier("This"),
      Identifier("is"),
      Identifier("not"),
      Identifier("a"),
      Identifier("comment")
    )

    expectNoDiagnostics("// comment")
  }

  it should "skip multi-line comments correctly" in {
    expectTokenSequence(
      "/* This is a multi-line comment \n \n Part of comment \n */ tokenise normally",
      Identifier("tokenise"),
      Identifier("normally")
    )

    expectTokenSequence("/*/ Doesn't close comment")

    // Unclosed multiline should also work
    expectTokenSequence(
      "class A /* Multi-line comments can begin after // \n \n // \n /* 123.456",
      Class,
      Identifier("A")
    )

    expectTokenSequence(
      "/* Comment /* */ comment ended",
      Identifier("comment"),
      Identifier("ended")
    )

    expectTokenSequence("/**/")
    expectTokenSequence("//**/ A single line comment")

    expectNoDiagnostics("/* comment */")
  }

  it should "output correct source ranges" in {
    val strs = List(
      // Basic tokens
      "( ) { } [ ]",
      ": ; ,",
      "+ - * / %",
      "< > = !",
      "| ^ ? .",

      // Multi-character operators
      "== != <= >=",
      "+= -= *= /= %=",
      "&= |= ^= <<= >>=",
      "++ -- && ||",
      "<< >>",

      // Keywords
      "class interface",
      "if else for while",
      "return",
      "true false null",
      "public private protected",
      "static final open abstract",

      // Identifiers and literals
      "abc _var var123 _123",
      "MyClass someFunction CONSTANT",
      "123 456 789",
      "12.34 0.0 999.999",
      "\"hello\" \"world with spaces\"",

      // Mixed expressions
      "x = 42",
      "array[index]",
      "obj.method()",
      "a + b * c",
      "if (x > 0) return true",
      "class MyClass : BaseClass",
      "var x = \"hello world\"",
      "a += b++",
      "x << 2 | y",
      "!valid && ready",

      // Numeric edge cases
      "0 1 999",
      "-42 +17",

      // String and character literals
      "\"\" \"a\" \"longer string\"",
      "\"string\"",

      // Operator combinations
      "x++ + ++y",
      "a-- - --b",
      "x && y || z",
      "a << b >> c",
      "x ? y : z",

      // Function and class syntax
      "class A { }",
      "var arr = [ 1 , 2 , 3 ]",
      "obj . prop = value",
      "func ( arg1 , arg2 )",

      // Control flow
      "if ( condition ) { }",
      "for ( i = 0 ; i < 10 ; i++ )",
      "while ( running ) continue",

      // Single characters that could be confused
      "a b c",
      "1 2 3",
      ". , ;",
      "( ) { } [ ]",

      // Empty and minimal cases
      "x",
      "42",
      "\"\"",
      "true",

      // Boundary cases
      "+ +",
      "- -",
      "< <",
      "> >",
      "= =",
      "! !",
      "& &",
      "| |",

      // Multi space
      "a   b",
      "a   b  c d  e"
    )

    strs.foreach { str =>
      val (tokenInput, _) = Tokeniser.tokenise(str, 0)
      val tokens = tokenInput.source
      val reconstructed = unTokenise(tokens)
      withClue(s"Input: '$str', Reconstructed: '$reconstructed': ") {
        reconstructed shouldBe str
      }
    }
  }
}
