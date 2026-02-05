package ast

import sources.SourceRange

import scala.annotation.targetName
import symbols.{Scope, Symbol}

sealed trait AstNode extends PrettyPrintable {
  def range: SourceRange
}

trait PrettyPrintable {
  def prettyPrint: String
}

// ===== Expressions =====

sealed trait Expr extends AstNode

sealed trait LiteralExpr extends Expr

case class BoolLiteral(value: Boolean, range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = value.toString
}

case class FloatLiteral(value: Float, range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = value.toString
}

case class IntegerLiteral(value: Int, range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = value.toString
}

case class NullLiteral(range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = "null"
}

case class StringLiteral(value: String, range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = value
}

case class Ident(head: String, tail: List[String] = List.empty, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = (head :: tail).mkString(".")

  def name: String = (head :: tail).last
}

enum BinaryOp extends PrettyPrintable {
  case EQUALS // "="
  case DOUBLE_EQUALS // "=="
  case LESS // "<"
  case GREATER // ">"
  case LESS_EQUALS // "<="
  case GREATER_EQUALS // ">="
  case NOT_EQUALS // "!="
  case PLUS // "+"
  case MINUS // "-"
  case STAR // "*"
  case SLASH // "/"
  case MODULO // "%"
  case PLUS_EQUALS // "+="
  case MINUS_EQUALS // "-="
  case STAR_EQUALS // "*="
  case SLASH_EQUALS // "/="
  case MODULO_EQUALS // "%="
  case LEFT_SHIFT_EQUALS // "<<="
  case RIGHT_SHIFT_EQUALS // ">>="
  case BITWISE_AND_EQUALS // "&="
  case BITWISE_OR_EQUALS // "|="
  case BITWISE_XOR_EQUALS // "^="
  case LOGICAL_AND // "&&"
  case LOGICAL_OR // "||"
  case BITWISE_AND // "&"
  case BITWISE_OR // "|"
  case BITWISE_XOR // "^"
  case LEFT_SHIFT // "<<"
  case RIGHT_SHIFT // ">>"

  override def prettyPrint: String = this match {
    case BinaryOp.EQUALS => "="
    case BinaryOp.DOUBLE_EQUALS => "=="
    case BinaryOp.LESS => "<"
    case BinaryOp.GREATER => ">"
    case BinaryOp.LESS_EQUALS => "<="
    case BinaryOp.GREATER_EQUALS => ">="
    case BinaryOp.NOT_EQUALS => "!="
    case BinaryOp.PLUS => "+"
    case BinaryOp.MINUS => "-"
    case BinaryOp.STAR => "*"
    case BinaryOp.SLASH => "/"
    case BinaryOp.MODULO => "%"
    case BinaryOp.PLUS_EQUALS => "+="
    case BinaryOp.MINUS_EQUALS => "-="
    case BinaryOp.STAR_EQUALS => "*="
    case BinaryOp.SLASH_EQUALS => "/="
    case BinaryOp.MODULO_EQUALS => "%="
    case BinaryOp.LEFT_SHIFT_EQUALS => "<<="
    case BinaryOp.RIGHT_SHIFT_EQUALS => ">>="
    case BinaryOp.BITWISE_AND_EQUALS => "&="
    case BinaryOp.BITWISE_OR_EQUALS => "|="
    case BinaryOp.BITWISE_XOR_EQUALS => "^="
    case BinaryOp.LOGICAL_AND => "&&"
    case BinaryOp.LOGICAL_OR => "||"
    case BinaryOp.BITWISE_AND => "&"
    case BinaryOp.BITWISE_OR => "|"
    case BinaryOp.BITWISE_XOR => "^"
    case BinaryOp.LEFT_SHIFT => "<<"
    case BinaryOp.RIGHT_SHIFT => ">>"
  }
}

case class BinaryExpr(expr1: Expr, expr2: Expr, op: BinaryOp, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"(${expr1.prettyPrint} ${op.prettyPrint} ${expr2.prettyPrint})"
}

enum UnaryOp extends PrettyPrintable {
  case NOT // "!"
  case PLUS // "+"
  case MINUS // "-"
  case POST_INCREMENT // "++"
  case POST_DECREMENT // "--"
  case PRE_INCREMENT // "++"
  case PRE_DECREMENT // "--"

  override def prettyPrint: String = this match {
    case UnaryOp.NOT => "!"
    case UnaryOp.PLUS => "+"
    case UnaryOp.MINUS => "-"
    case UnaryOp.POST_INCREMENT => "++"
    case UnaryOp.POST_DECREMENT => "--"
    case UnaryOp.PRE_INCREMENT => "++"
    case UnaryOp.PRE_DECREMENT => "--"
  }
}

case class UnaryExpr(expr: Expr, op: UnaryOp, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = op match {
    case UnaryOp.POST_INCREMENT | UnaryOp.POST_DECREMENT => s"(${expr.prettyPrint}${op.prettyPrint})"
    case _ => s"(${op.prettyPrint}${expr.prettyPrint})"
  }
}

case class CallExpr(callee: Expr, args: List[Expr], range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"${callee.prettyPrint}${args.map(_.prettyPrint).mkString("(", ", ", ")")}"
}

case class IndexExpr(callee: Expr, arg: Expr, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"${callee.prettyPrint}[${arg.prettyPrint}]"
}

case class MemberAccessExpr(base: Expr, member: String, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"${base.prettyPrint}.$member"
}

case class TernaryExpr(cond: Expr, expr1: Expr, expr2: Expr, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"${cond.prettyPrint} ? ${expr1.prettyPrint} : ${expr2.prettyPrint}"
}

// ===== Statements =====

sealed trait Stmt extends AstNode

case class BreakStmt(range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = "break;"
}

case class ContinueStmt(range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = "continue;"
}

case class ExprStmt(expr: Expr, range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = s"${expr.prettyPrint};"
}

case class BlockStmt(stmts: List[Stmt], range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = stmts.map(_.prettyPrint).mkString("{\n", "\n", "\n}\n")
  val scope: Scope = Scope()
}

case class IfStmt(expr: Expr, ifBlock: BlockStmt, elseBlock: Option[BlockStmt] = None, range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = s"if (${expr.prettyPrint}) ${ifBlock.prettyPrint}${
    elseBlock match {
      case Some(block) => s" else ${block.prettyPrint}"
      case None => ""
    }}"
}

case class ReturnStmt(expr: Expr, range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = s"return ${expr.prettyPrint};"
}

case class WhileStmt(cond: Expr, body: BlockStmt, range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = s"while (${cond.prettyPrint}) ${body.prettyPrint}"
}

case class VariableDeclStmt(variableDecl: VariableDecl, range: SourceRange = SourceRange.dummy) extends Stmt {
  override def prettyPrint: String = variableDecl.prettyPrint
}

// TODO - For loop


// ===== Types =====

enum Variance extends PrettyPrintable {
  case COVARIANT
  case CONTRAVARIANT
  case INVARIANT

  override def prettyPrint: String = this match {
    case Variance.COVARIANT => "out "
    case Variance.CONTRAVARIANT => "in "
    case Variance.INVARIANT => ""
  }
}

case class TypeRef(name: Ident, args: List[(TypeRef, Variance)], range: SourceRange = SourceRange.dummy) extends AstNode {
  override def prettyPrint: String = {
    val prettyArgs = if args.nonEmpty then args.map {
      (typeRef, variance) => s"${variance.prettyPrint}${typeRef.prettyPrint}"
    }.mkString("[", ", ", "]")
    else ""

    s"${name.prettyPrint}$prettyArgs"
  }
}

// ===== OOP Nodes =====

enum Modifier extends PrettyPrintable {
  case OPEN
  case FINAL
  case ABSTRACT
  case PUBLIC
  case PRIVATE
  case PROTECTED
  case STATIC
  case OVERRIDE

  override def prettyPrint: String = this match {
    case Modifier.OPEN => "open"
    case Modifier.FINAL => "final"
    case Modifier.ABSTRACT => "abstract"
    case Modifier.PUBLIC => "public"
    case Modifier.PRIVATE => "private"
    case Modifier.PROTECTED => "protected"
    case Modifier.STATIC => "static"
    case Modifier.OVERRIDE => "override"
  }

  def isModality: Boolean = this match {
    case Modifier.OPEN | Modifier.FINAL | Modifier.ABSTRACT => true
    case _ => false
  }

  def isVisibility: Boolean = this match {
    case Modifier.PUBLIC | Modifier.PROTECTED | Modifier.PRIVATE => true
    case _ => false
  }
}

case class ModifierNode(modifier: Modifier, range: SourceRange = SourceRange.dummy) extends AstNode {
  override def prettyPrint: String = modifier.prettyPrint
}

sealed trait Decl extends AstNode {
  val name: String
  val modifiers: List[ModifierNode]
}

extension (modifiers: List[ModifierNode]) {
  def prettyPrint: String = if modifiers.isEmpty then "" else s"${modifiers.map(_.prettyPrint).mkString("", " ", " ")}"
}

extension (typeParameters: List[TypeParameterDecl]) {
  @targetName("typeParameterPrettyPrint")
  def prettyPrint: String = if typeParameters.nonEmpty then typeParameters.map(_.prettyPrint).mkString("[", ", ", "]")
  else ""
}

case class TypedefDecl(name: String,
                       typeParameters: List[TypeParameterDecl],
                       referredType: TypeRef, 
                       modifiers: List[ModifierNode] = List.empty,
                       range: SourceRange = SourceRange.dummy) extends Decl {
  override def prettyPrint: String = s"${modifiers.prettyPrint}typedef $name${typeParameters.prettyPrint} = ${referredType.prettyPrint};"
}

case class VariableDecl(name: String, 
                        typeRef: TypeRef, 
                        initExpr: Option[Expr] = None, 
                        modifiers: List[ModifierNode] = List.empty,
                        range: SourceRange = SourceRange.dummy) extends Decl {
  override def prettyPrint: String = s"${modifiers.prettyPrint}${typeRef.prettyPrint} $name${
    initExpr match {
      case Some(expr) => s" = ${expr.prettyPrint}"
      case None => ""
    }};"
}

case class TypeParameterDecl(name: String, variance: Variance, range: SourceRange = SourceRange.dummy, modifiers: List[ModifierNode] = List.empty) extends Decl {
  override def prettyPrint: String = s"${variance.prettyPrint}$name"
}

case class FunctionDecl(name: String, 
                        returnType: TypeRef, 
                        parameters: List[VariableDecl], 
                        block: BlockStmt, 
                        modifiers: List[ModifierNode] = List.empty, 
                        typeParameters: List[TypeParameterDecl] = List.empty,
                        range: SourceRange = SourceRange.dummy) extends Decl {
  override def prettyPrint: String = s"${modifiers.prettyPrint} ${returnType.prettyPrint} $name${parameters.map(_.prettyPrint.init).mkString("(", ", ", ")")}${block.prettyPrint}"
}

case class ClassDecl(name: String,
                     modifiers: List[ModifierNode] = List.empty,
                     superClasses: List[TypeRef] = List.empty,
                     fields: List[VariableDecl] = List.empty,
                     methods: List[FunctionDecl] = List.empty,
                     nestedClasses: List[ClassDecl] = List.empty,
                     typeParameters: List[TypeParameterDecl] = List.empty, 
                     range: SourceRange = SourceRange.dummy) extends Decl {
  override def prettyPrint: String = s"${modifiers.prettyPrint}class $name${typeParameters.prettyPrint}${
    if superClasses.isEmpty then ""
    else s": ${superClasses.map(_.prettyPrint).mkString(", ")}"
  } {\n${fields.map(_.prettyPrint).mkString("\n")}\n${methods.map(_.prettyPrint).mkString("\n")}\n${nestedClasses.map(_.prettyPrint).mkString("\n")}\n}"
}

case class KahwaFile(typedefDecls: List[TypedefDecl] = List.empty,
                     classDecls: List[ClassDecl] = List.empty,
                     functionDecls: List[FunctionDecl] = List.empty,
                     variableDecls: List[VariableDecl] = List.empty, 
                     range: SourceRange = SourceRange.dummy) extends Decl {
  override def prettyPrint: String
  = s"${typedefDecls.map(_.prettyPrint).mkString("\n")}\n${classDecls.map(_.prettyPrint).mkString("\n")}${functionDecls.map(_.prettyPrint).mkString("\n")}${variableDecls.map(_.prettyPrint).mkString("\n")}"

  override val name: String = "dummy"
  override val modifiers: List[ModifierNode] = List.empty
}
