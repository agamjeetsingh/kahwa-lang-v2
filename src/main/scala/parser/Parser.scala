package parser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.*
import sources.SourceRange

import scala.language.postfixOps
import parser.Parsel.*
import parser.Token.{Class, Identifier, Private, RightCurlyBrace, SemiColon, Typedef}
import parser.Parsel.list

object Parser {
  type ParserFunc[A] = ParserFunction[A, Token, Diagnostic]
  private type SafePointFunc = SafePointFunction[Token]

  val isSafePointForFile: SafePointFunc = {
    case Token.Identifier(_, _) | Token.Typedef(_) | Token.Class(_) | Token.Interface(_) =>
      true
    case t if t.isModifier => true
    case _ => false
  }

  val isSafePointForClass: SafePointFunc = {
    case Token.Identifier(_, _) => true
    case Token.RightCurlyBrace(_) => true
    case t if t.isModifier => true
    case _ => false
  }

  val skipNothing: SafePointFunc = _ => true

  val isSafePointForStmt: SafePointFunc = {
    case Token.Static(_) | Token.RightCurlyBrace(_) | Token.SemiColon(_) => true
    case _ => false
  }

  private val isSafePointForBlock: SafePointFunc = {
    case Token.RightCurlyBrace(_) | Token.SemiColon(_) => true
    case _ => false
  }

  private def parseToken[A](
      tokenMatch: Token => Option[A],
      notMatchError: Token => Diagnostic,
      endOfFileError: SourceRange => Diagnostic
  ): Parsel[A, Token, Diagnostic] = Parsel((input: Parsel.Input[Token]) =>
    input.current match {
      case Some(token) =>
        val matched = tokenMatch(token)
        matched match {
          case Some(value: A) => (matched, input.advance, Iterable.empty)
          case None => (None, input, List(notMatchError(token)))
        }
      case None =>
        (
          None,
          input,
          List(endOfFileError(input.last match {
            case Some(token) => token.range
            case None => SourceRange.dummy
          }))
        )
    }
  )

  private def parseTok[A](
      expected: String
  )(check: PartialFunction[Token, A]): Parsel[A, Token, Diagnostic] =
    parseToken(
      check.lift,
      token => ExpectedSomething(expected, token.prettyPrint, token.range),
      range => ExpectedSomething(expected, "EOF", range)
    )

  private val parseModifierNode: Parsel[ModifierNode, Token, Diagnostic] =
    parseToken(
      token =>
        if (token.isModifier) {
          Some(
            ModifierNode(
              token match {
                case Token.Static(_) => Modifier.STATIC
                case Token.Public(_) => Modifier.PUBLIC
                case Token.Private(_) => Modifier.PRIVATE
                case Token.Protected(_) => Modifier.PROTECTED
                case Token.Open(_) => Modifier.OPEN
                case Token.Final(_) => Modifier.FINAL
                case Token.Abstract(_) => Modifier.ABSTRACT
                case Token.Override(_) => Modifier.OVERRIDE
                case _ =>
                  throw IllegalStateException(
                    "Unreachable in parseModifierNode()"
                  )
              },
              token.range
            )
          )
        } else {
          None
        },
      token => ExpectedSomething("modifier", token.prettyPrint, token.range),
      range => ExpectedSomething("modifier", "EOF", range)
    )

  lazy val parseTypeRef: SafePointFunc ?=> Parsel[TypeRef, Token, Diagnostic] = {
    spanned(
      parseIdentifier ~ optional(
        parseDot ~> sepBy(parseIdentifier, parseDot)
      ) ~ optional(
        parseLeftBracket ~> sepBy(
          parseVariance ~ delay(parseTypeRef),
          parseComma
        ) <~ parseRightBracket
      )
    ).map { tuple =>
      val (((head, optionalTail), optionalArgs), range) = tuple
      // TODO - verify
      val unqualRange =
        (if (optionalTail.toList.flatten.isEmpty) head.range
         else optionalTail.toList.flatten.last.range) <-> head.range
      optionalArgs match {
        case Some(args) =>
          AtomType(
            Ident(
              head.value,
              optionalTail.getOrElse(List.empty).map(_.value),
              unqualRange
            ),
            args.map(_.swap),
            range
          )
        case None =>
          AtomType(
            Ident(
              head.value,
              optionalTail.getOrElse(List.empty).map(_.value),
              unqualRange
            ),
            List.empty,
            range
          )
      }
    }
  }

  lazy val parseTypedefDecl: SafePointFunc ?=> Parsel[TypedefDecl, Token, Diagnostic] = {
    spanned(
      (list(parseModifierNode) <~ parseTypedef) ~ commit(
        parseIdentifier ~ (optional(
          parseGenericArguments
        ) <~ parseEquals) ~ parseTypeRef <~ parseSemiColon
      )
    ).map { tuple =>
      val (
        (modifierNodes, ((identifier, optionalGenericArgs), typeRef)),
        range
      ) = tuple
      TypedefDecl(
        identifier.value,
        optionalGenericArgs.getOrElse(List.empty),
        typeRef,
        modifierNodes,
        range
      )
    }
  }

  lazy val parseVariance: SafePointFunc ?=> Parsel[Variance, Token, Diagnostic] = {
    optional(or(parseOut, parseIn)).map {
      case None => Variance.INVARIANT
      case Some(_: Token.Out) => Variance.COVARIANT
      case Some(_: Token.In) => Variance.CONTRAVARIANT
    }
  }

  lazy val atomExpr: SafePointFunc ?=> Parsel[Expr, Token, Diagnostic] = {
    or(
      parseTrue.map(tok => BoolLiteral(true, tok.range)),
      parseFalse.map(tok => BoolLiteral(false, tok.range)),
      parseNull.map(tok => NullLiteral(tok.range)),
      parseFloat.map(tok => FloatLiteral(tok.value, tok.range)),
      parseInteger.map(tok => IntegerLiteral(tok.value, tok.range)),
      parseStringLiteral.map(tok => StringLiteral(tok.value, tok.range)),
      parseIdentifier.map(tok => Ident(tok.value, List.empty, tok.range))
    )
  }

  lazy val parseExpr: SafePointFunc ?=> Parsel[Expr, Token, Diagnostic] = {
    precedence[Expr, Token, Diagnostic](
      atomExpr,
      parseLeftParen ~> delay(parseExpr) <~ parseRightParen
    )(
      Ops(InfixR)(
        parseEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.EQUALS, l.range <-> r.range)),
        parsePlusEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.PLUS_EQUALS, l.range <-> r.range)),
        parseMinusEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.MINUS_EQUALS, l.range <-> r.range)),
        parseStarEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.STAR_EQUALS, l.range <-> r.range)),
        parseSlashEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.SLASH_EQUALS, l.range <-> r.range)),
        parseModuloEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.MODULO_EQUALS, l.range <-> r.range)),
        parseLeftShiftEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LEFT_SHIFT_EQUALS, l.range <-> r.range)
        ),
        parseRightShiftEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.RIGHT_SHIFT_EQUALS, l.range <-> r.range)
        ),
        parseBitwiseAndEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_AND_EQUALS, l.range <-> r.range)
        ),
        parseBitwiseOrEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_OR_EQUALS, l.range <-> r.range)
        ),
        parseBitwiseXorEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_XOR_EQUALS, l.range <-> r.range)
        )
      ),
      Ops(InfixL)(
        parseLogicalOr.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LOGICAL_OR, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseLogicalAnd.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LOGICAL_AND, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseBitwiseOr.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_OR, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseBitwiseXor.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_XOR, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseBitwiseAnd.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.BITWISE_AND, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseDoubleEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.DOUBLE_EQUALS, l.range <-> r.range)),
        parseNotEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.NOT_EQUALS, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseLessEquals.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LESS_EQUALS, l.range <-> r.range)),
        parseGreaterEquals.map(_ =>
          (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.GREATER_EQUALS, l.range <-> r.range)
        ),
        parseLess.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LESS, l.range <-> r.range)),
        parseGreater.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.GREATER, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseLeftShift.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.LEFT_SHIFT, l.range <-> r.range)),
        parseRightShift.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.RIGHT_SHIFT, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parsePlus.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.PLUS, l.range <-> r.range)),
        parseMinus.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.MINUS, l.range <-> r.range))
      ),
      Ops(InfixL)(
        parseStar.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.STAR, l.range <-> r.range)),
        parseSlash.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.SLASH, l.range <-> r.range)),
        parseModulo.map(_ => (l: Expr, r: Expr) => BinaryExpr(l, r, BinaryOp.MODULO, l.range <-> r.range))
      ),
      Ops(Prefix)(
        parsePlus.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.PLUS, tok.range <-> e.range)),
        parseMinus.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.MINUS, tok.range <-> e.range)),
        parseNot.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.NOT, tok.range <-> e.range)),
        parseIncrement.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.PRE_INCREMENT, tok.range <-> e.range)),
        parseDecrement.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.PRE_DECREMENT, tok.range <-> e.range))
      ),
      Ops(Postfix)(
        parseIncrement.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.POST_INCREMENT, tok.range <-> e.range)),
        parseDecrement.map(tok => (e: Expr) => UnaryExpr(e, UnaryOp.POST_DECREMENT, tok.range <-> e.range)),
        spanned(parseLeftBracket ~> delay(parseExpr) <~ parseRightBracket).map(tuple =>
          val (arg, range) = tuple
          (e: Expr) => IndexExpr(e, arg, e.range <-> range)
        ),
        spanned(
          parseLeftParen ~> sepBy(
            delay(parseExpr),
            parseComma
          ) <~ parseRightParen
        ).map(tuple =>
          val (args, range) = tuple
          (e: Expr) => CallExpr(e, args, e.range <-> range)
        )
      ),
      Ops(Postfix)(
        (parseDot ~> parseIdentifier).map(ident =>
          (e: Expr) => MemberAccessExpr(e, ident.value, e.range <-> ident.range)
        )
      )
    )
  }

  lazy val parseBlock: SafePointFunc ?=> Parsel[BlockStmt, Token, Diagnostic] = {
    given SafePointFunc = isSafePointForBlock
    spanned(
      parseLeftCurlyBrace ~> list(delay(parseStmt)) <~ parseRightCurlyBrace
    ).map(tuple =>
      val (stmts, range) = tuple
      BlockStmt(stmts, range)
    )
  }

  lazy val parseStmt: SafePointFunc ?=> Parsel[Stmt, Token, Diagnostic] = {
    or(
      (parseBreak <~ parseSemiColon).map(tok => BreakStmt(tok.range)),
      (parseContinue <~ parseSemiColon).map(tok => ContinueStmt(tok.range)),
      (parseExpr <~ parseSemiColon).map(expr => ExprStmt(expr, expr.range)),
      parseBlock,
      spanned(
        (parseIf ~> parseLeftParen ~> parseExpr <~ parseRightParen) ~ parseBlock ~ optional(
          parseElse ~> parseBlock
        )
      ).map(tuple =>
        val (((expr, ifBlock), optionalElseBlock), range) = tuple
        IfStmt(expr, ifBlock, optionalElseBlock, range)
      ),
      spanned(parseReturn ~> parseExpr <~ parseSemiColon).map(tuple =>
        val (expr, range) = tuple
        ReturnStmt(expr, range)
      ),
      spanned(
        (parseWhile ~> parseLeftParen ~> parseExpr <~ parseRightParen) ~ parseBlock
      ).map(tuple =>
        val ((expr, blockStmt), range) = tuple
        WhileStmt(expr, blockStmt, range)
      ),
      spanned(
        list(parseModifierNode) ~ parseTypeRef ~ parseIdentifier ~ (optional(
          parseEquals ~> parseExpr
        ) <~ parseSemiColon)
      ).map(tuple => {
        val ((((modifierNodes, varType), identifier), optionalExpr), range) =
          tuple
        VariableDeclStmt(
          VariableDecl(
            identifier.value,
            varType,
            optionalExpr,
            modifierNodes,
            range
          )
        )
      })
    )
  }

  lazy val parseGenericArguments: SafePointFunc ?=> Parsel[List[TypeParameterDecl], Token, Diagnostic] = {
    (parseLeftBracket ~> sepBy(
      parseVariance ~ parseTypeRef,
      parseComma
    ) <~ parseRightBracket).map(genericArgs => {
      genericArgs.map((variance, typeRef) => TypeParameterDecl(typeRef.name.prettyPrint, variance))
    })
  }

  lazy val parseFunctionDecl: SafePointFunc ?=> Parsel[FunctionDecl, Token, Diagnostic] = {
    spanned(
      list(parseModifierNode) ~ parseTypeRef ~ parseIdentifier ~
        (parseLeftParen ~> sepBy(
          parseVariableDecl,
          parseComma
        ) <~ parseRightParen) ~ parseBlock
    ).map(tuple => {
      val (((((modifiers, returnType), identifier), parameters), body), range) =
        tuple
      FunctionDecl(
        identifier.value,
        returnType,
        parameters,
        body,
        modifiers,
        List.empty,
        range
      )
    })
  }

  lazy val parseVariableDecl: SafePointFunc ?=> Parsel[VariableDecl, Token, Diagnostic] = {
    spanned(
      list(parseModifierNode) ~ parseTypeRef ~ parseIdentifier ~ optional(
        parseEquals ~> parseExpr
      )
    ).map(tuple => {
      val ((((modifierNodes, varType), identifier), optionalExpr), range) =
        tuple
      VariableDecl(
        identifier.value,
        varType,
        optionalExpr,
        modifierNodes,
        range
      )
    })
  }

  lazy val parseClassDecl: SafePointFunc ?=> Parsel[ClassDecl, Token, Diagnostic] = {
    spanned(
      (list(parseModifierNode) <~ parseClass) ~ parseIdentifier ~
        optional(parseGenericArguments) ~
        optional(parseColon ~> sepBy(parseTypeRef, parseComma)) ~
        (parseLeftCurlyBrace ~> list(
          or(
            delay(parseClassDecl),
            parseFunctionDecl,
            parseVariableDecl <~ parseSemiColon
          )
        ) <~ parseRightCurlyBrace)
    ).map(tuple => {
      val (
        (
          (
            ((modifierNodes, identifier), optionalTypeParameters),
            optionalSuperClasses
          ),
          classMembers
        ),
        range
      ) = tuple
      ClassDecl(
        identifier.value,
        modifierNodes,
        optionalSuperClasses.getOrElse(Nil),
        classMembers.collect { case decl: VariableDecl => decl },
        classMembers.collect { case decl: FunctionDecl => decl },
        classMembers.collect { case decl: ClassDecl => decl },
        optionalTypeParameters.getOrElse(Nil),
        range
      )
    })
  }

  lazy val parseKahwaFile: SafePointFunc ?=> Parsel[KahwaFile, Token, Diagnostic] = {
    fully(
      or(
        (input: Input[Token]) => {
          (list(parseModifierNode) <~ parseTypedef)(input)._1 match {
            case Some(_) => Some(2)
            case None => {
              (list(parseModifierNode) <~ parseClass)(input)._1 match {
                case Some(_) => Some(0)
                case None => {
                  (list(parseModifierNode) ~ parseTypeRef ~ parseIdentifier ~
                    parseLeftParen)(input)._1 match {
                    case Some(_) => Some(1)
                    case None => {
                      (list(
                        parseModifierNode
                      ) ~ parseTypeRef ~ parseIdentifier ~ or(
                        parseSemiColon,
                        parseEquals
                      ))(input)._1 match {
                        case Some(_) => Some(3)
                        case None => Some(-1)
                      }
                    }
                  }
                }
              }
            }
          }
        },
        (
          0 -> parseClassDecl,
          1 -> parseFunctionDecl,
          2 -> parseTypedefDecl,
          3 -> (parseVariableDecl <~ parseSemiColon)
        ),
        _ => Iterable[Diagnostic]().empty
      )
    ).map(fileMembers => {
      KahwaFile(
        fileMembers.collect { case decl: TypedefDecl => decl },
        fileMembers.collect { case decl: ClassDecl => decl },
        fileMembers.collect { case decl: FunctionDecl => decl },
        fileMembers.collect { case decl: VariableDecl => decl },
        SourceRange.dummy // TODO
      )
    })
  }

  // Pre-defined token parsers
  private val parseColon = parseTok(":") { case tok: Token.Colon => tok }
  private val parseSemiColon = parseTok(";") { case tok: Token.SemiColon =>
    tok
  }
  private val parseComma = parseTok(",") { case tok: Token.Comma => tok }
  private val parseLeftCurlyBrace = parseTok("{") { case tok: Token.LeftCurlyBrace =>
    tok
  }
  private val parseRightCurlyBrace = parseTok("}") { case tok: Token.RightCurlyBrace =>
    tok
  }
  private val parseLeftParen = parseTok("(") { case tok: Token.LeftParen =>
    tok
  }
  private val parseRightParen = parseTok(")") { case tok: Token.RightParen =>
    tok
  }
  private val parseLeftBracket = parseTok("[") { case tok: Token.LeftBracket =>
    tok
  }
  private val parseRightBracket = parseTok("]") { case tok: Token.RightBracket =>
    tok
  }

  private val parseEquals = parseTok("=") { case tok: Token.Equals => tok }
  private val parseDoubleEquals = parseTok("==") { case tok: Token.DoubleEquals =>
    tok
  }
  private val parseLess = parseTok("<") { case tok: Token.Less => tok }
  private val parseGreater = parseTok(">") { case tok: Token.Greater => tok }
  private val parseLessEquals = parseTok("<=") { case tok: Token.LessEquals =>
    tok
  }
  private val parseGreaterEquals = parseTok(">=") { case tok: Token.GreaterEquals =>
    tok
  }
  private val parseNot = parseTok("!") { case tok: Token.Not => tok }
  private val parseNotEquals = parseTok("!=") { case tok: Token.NotEquals =>
    tok
  }
  private val parsePlus = parseTok("+") { case tok: Token.Plus => tok }
  private val parseMinus = parseTok("-") { case tok: Token.Minus => tok }
  private val parseStar = parseTok("*") { case tok: Token.Star => tok }
  private val parseSlash = parseTok("/") { case tok: Token.Slash => tok }
  private val parseModulo = parseTok("%") { case tok: Token.Modulo => tok }
  private val parsePlusEquals = parseTok("+=") { case tok: Token.PlusEquals =>
    tok
  }
  private val parseMinusEquals = parseTok("-=") { case tok: Token.MinusEquals =>
    tok
  }
  private val parseStarEquals = parseTok("*=") { case tok: Token.StarEquals =>
    tok
  }
  private val parseSlashEquals = parseTok("/=") { case tok: Token.SlashEquals =>
    tok
  }
  private val parseModuloEquals = parseTok("%=") { case tok: Token.ModuloEquals =>
    tok
  }
  private val parseLeftShiftEquals = parseTok("<<=") { case tok: Token.LeftShiftEquals =>
    tok
  }
  private val parseRightShiftEquals = parseTok(">>=") { case tok: Token.RightShiftEquals =>
    tok
  }
  private val parseBitwiseAndEquals = parseTok("&=") { case tok: Token.BitwiseAndEquals =>
    tok
  }
  private val parseBitwiseOrEquals = parseTok("|=") { case tok: Token.BitwiseOrEquals =>
    tok
  }
  private val parseBitwiseXorEquals = parseTok("^=") { case tok: Token.BitwiseXorEquals =>
    tok
  }
  private val parseIncrement = parseTok("++") { case tok: Token.Increment =>
    tok
  }
  private val parseDecrement = parseTok("--") { case tok: Token.Decrement =>
    tok
  }
  private val parseLogicalAnd = parseTok("&&") { case tok: Token.LogicalAnd =>
    tok
  }
  private val parseLogicalOr = parseTok("||") { case tok: Token.LogicalOr =>
    tok
  }
  private val parseBitwiseAnd = parseTok("&") { case tok: Token.BitwiseAnd =>
    tok
  }
  private val parseBitwiseOr = parseTok("|") { case tok: Token.BitwiseOr =>
    tok
  }
  private val parseBitwiseXor = parseTok("^") { case tok: Token.BitwiseXor =>
    tok
  }
  private val parseLeftShift = parseTok("<<") { case tok: Token.LeftShift =>
    tok
  }
  private val parseRightShift = parseTok(">>") { case tok: Token.RightShift =>
    tok
  }
  private val parseQuestion = parseTok("?") { case tok: Token.Question => tok }
  private val parseDot = parseTok(".") { case tok: Token.Dot => tok }

  private val parseClass = parseTok("class") { case tok: Token.Class => tok }
  private val parseStatic = parseTok("static") { case tok: Token.Static => tok }
  private val parsePublic = parseTok("public") { case tok: Token.Public => tok }
  private val parsePrivate = parseTok("private") { case tok: Token.Private =>
    tok
  }
  private val parseProtected = parseTok("protected") { case tok: Token.Protected =>
    tok
  }
  private val parseOpen = parseTok("open") { case tok: Token.Open => tok }
  private val parseFinal = parseTok("final") { case tok: Token.Final => tok }
  private val parseAbstract = parseTok("abstract") { case tok: Token.Abstract =>
    tok
  }
  private val parseInterface = parseTok("interface") { case tok: Token.Interface =>
    tok
  }
  private val parseTypedef = parseTok("typedef") { case tok: Token.Typedef =>
    tok
  }
  private val parseIn = parseTok("in") { case tok: Token.In => tok }
  private val parseOut = parseTok("out") { case tok: Token.Out => tok }
  private val parseOverride = parseTok("override") { case tok: Token.Override =>
    tok
  }

  private val parseReturn = parseTok("return") { case tok: Token.Return => tok }
  private val parseIf = parseTok("if") { case tok: Token.If => tok }
  private val parseElse = parseTok("else") { case tok: Token.Else => tok }
  private val parseFor = parseTok("for") { case tok: Token.For => tok }
  private val parseWhile = parseTok("while") { case tok: Token.While => tok }
  private val parseBreak = parseTok("break") { case tok: Token.Break => tok }
  private val parseContinue = parseTok("continue") { case tok: Token.Continue =>
    tok
  }

  private val parseTrue = parseTok("true") { case tok: Token.True => tok }
  private val parseFalse = parseTok("false") { case tok: Token.False => tok }
  private val parseNull = parseTok("null") { case tok: Token.NullLiteral =>
    tok
  }

  private val parseIdentifier = parseTok("identifier") { case tok: Token.Identifier =>
    tok
  }
  private val parseStringLiteral = parseTok("string literal") { case tok: Token.StringLiteral =>
    tok
  }
  private val parseCharLiteral = parseTok("char literal") { case tok: Token.CharLiteral =>
    tok
  }
  private val parseInteger = parseTok("integer") { case tok: Token.IntegerLiteral =>
    tok
  }
  private val parseFloat = parseTok("float") { case tok: Token.FloatLiteral =>
    tok
  }

  private def spanned[A, T, Error](
      parser: Parsel[A, T, Error]
  ): Parsel[(A, SourceRange), T, Error] = {
    Parsel((input: Input[T]) => {
      val (res, next, errs) = parser(input)
      res match {
        case Some(a) =>
          val range = if (input.index < next.index) {
            // Get range from first to last consumed token
            val firstToken = input.source(input.index).asInstanceOf[Token]
            val lastToken = input.source(next.index - 1).asInstanceOf[Token]
            firstToken.range <-> lastToken.range
          } else {
            SourceRange.dummy
          }
          (Some((a, range)), next, errs)
        case None => (None, next, errs)
      }
    })
  }
}
