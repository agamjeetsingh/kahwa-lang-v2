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

case class StringLiteral(value: String, range: SourceRange = SourceRange.dummy) extends LiteralExpr {
  override def prettyPrint: String = s"\"$value\""
}

case class Ident(
    head: String,
    tail: List[String] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends Expr {
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

case class BinaryExpr(
    expr1: Expr,
    expr2: Expr,
    op: BinaryOp,
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String =
    s"(${expr1.prettyPrint} ${op.prettyPrint} ${expr2.prettyPrint})"
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

case class UnaryExpr(
    expr: Expr,
    op: UnaryOp,
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String = op match {
    case UnaryOp.POST_INCREMENT | UnaryOp.POST_DECREMENT =>
      s"(${expr.prettyPrint}${op.prettyPrint})"
    case _ => s"(${op.prettyPrint}${expr.prettyPrint})"
  }
}

case class CallExpr(
    callee: Expr,
    args: List[Expr],
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String =
    s"${callee.prettyPrint}${args.map(_.prettyPrint).mkString("(", ", ", ")")}"
}

case class MemberAccessExpr(
    base: Expr,
    member: String,
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String = s"${base.prettyPrint}.$member"
}

case class BlockExpr(exprs: List[Expr], range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = {
    def needsSemicolon(e: Expr) = e match {
      case _: BlockExpr | _: IfExpr | _: WhileExpr => false
      case _ => true
    }
    exprs
      .map(e =>
        if (needsSemicolon(e)) s"${e.prettyPrint};"
        else e.prettyPrint
      )
      .mkString("{\n", "\n", "\n}")
  }
  val scope: Scope = Scope()
}

case class IfExpr(
    expr: Expr,
    ifBlock: BlockExpr,
    elseBlock: Option[BlockExpr] = None,
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String =
    s"if (${expr.prettyPrint}) ${ifBlock.prettyPrint}${elseBlock match {
        case Some(block) => s" else ${block.prettyPrint}"
        case None => ""
      }}"
}

case class WhileExpr(
    cond: Expr,
    body: BlockExpr,
    range: SourceRange = SourceRange.dummy
) extends Expr {
  override def prettyPrint: String = {
    s"while (${cond.prettyPrint}) ${body.prettyPrint}"
  }
}

case class BreakExpr(range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = "break"
}

case class ContinueExpr(range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = "continue"
}

// TODO - Not sure how the parameter types are determined
case class LambdaExpr(paramList: List[VariableDecl], body: Expr, range: SourceRange = SourceRange.dummy) extends Expr {
  override def prettyPrint: String = s"((${paramList.map(_.prettyPrintParam).mkString(", ")}) => ${body.prettyPrint})"
}

case class TupleExpr(elements: List[Expr], range: SourceRange) extends Expr {
  require(elements.size >= 2)
  override def prettyPrint: String = elements.map(_.prettyPrint).mkString("(", ", ", ")")
}

// ===== Types =====

enum Variance extends PrettyPrintable {
  case COVARIANT
  case CONTRAVARIANT
  case INVARIANT

  override def prettyPrint: String = this match {
    case Variance.COVARIANT => "+"
    case Variance.CONTRAVARIANT => "-"
    case Variance.INVARIANT => ""
  }
}

sealed trait TypeRef extends AstNode

case class AtomType(
    name: Ident,
    args: List[TypeRef] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends TypeRef {
  override def prettyPrint: String = {
    val prettyArgs =
      if args.nonEmpty then args.map(_.prettyPrint).mkString("[", ", ", "]")
      else ""

    s"${name.prettyPrint}$prettyArgs"
  }
}

case class TupleType(
    elems: List[TypeRef],
    range: SourceRange = SourceRange.dummy
) extends TypeRef {
  override def prettyPrint: String =
    s"(${elems.map(_.prettyPrint).mkString(", ")})"
}

case class FunctionType(paramList: List[TypeRef], returnType: TypeRef, range: SourceRange = SourceRange.dummy)
    extends TypeRef {
  override def prettyPrint: String =
    s"((${paramList.map(_.prettyPrint).mkString(", ")}) => ${returnType.prettyPrint})"
}

// ===== OOP Nodes =====

enum Modifier extends PrettyPrintable {
  case OPEN
  case FINAL
  case ABSTRACT
  case PUBLIC
  case PRIVATE
  case PROTECTED
  case OVERRIDE

  override def prettyPrint: String = this match {
    case Modifier.OPEN => "open"
    case Modifier.FINAL => "final"
    case Modifier.ABSTRACT => "abstract"
    case Modifier.PUBLIC => "public"
    case Modifier.PRIVATE => "private"
    case Modifier.PROTECTED => "protected"
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

case class ModifierNode(
    modifier: Modifier,
    range: SourceRange = SourceRange.dummy
) extends AstNode {
  override def prettyPrint: String = modifier.prettyPrint
}

sealed trait Decl extends AstNode {
  val name: String
  val modifiers: List[ModifierNode]
}

extension (modifiers: List[ModifierNode]) {
  def prettyPrint: String = if modifiers.isEmpty then ""
  else s"${modifiers.map(_.prettyPrint).mkString("", " ", " ")}"
}

extension (typeParameters: List[TypeParameterDecl]) {
  @targetName("typeParameterPrettyPrint")
  def prettyPrint: String = if typeParameters.nonEmpty then typeParameters.map(_.prettyPrint).mkString("[", ", ", "]")
  else ""
}

case class TypedefDecl(
    name: String,
    typeParameters: List[TypeParameterDecl],
    referredType: TypeRef,
    modifiers: List[ModifierNode] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends Decl {
  override def prettyPrint: String =
    s"${modifiers.prettyPrint}typedef $name${typeParameters.prettyPrint} = ${referredType.prettyPrint};"
}

case class VariableDecl(
    name: String,
    typeRef: Option[TypeRef],
    readOnly: Boolean,
    initExpr: Option[Expr] = None,
    range: SourceRange = SourceRange.dummy
) extends Decl,
      Expr {
  override def prettyPrint: String =
    s"${if readOnly then "val" else "var"} $name${typeRef.map(typeRef => s": ${typeRef.prettyPrint}").getOrElse("")}${initExpr.map(expr => s" = ${expr.prettyPrint}").getOrElse("")};"

  def prettyPrintParam: String =
    s"$name${typeRef.map(typeRef => s": ${typeRef.prettyPrint}").getOrElse("")}"

  override val modifiers: List[ModifierNode] = List.empty
}

case class FieldDecl(
    name: String,
    typeRef: Option[TypeRef],
    readOnly: Boolean,
    initExpr: Option[Expr] = None,
    modifiers: List[ModifierNode] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends Decl {
  override def prettyPrint: String =
    s"${modifiers.prettyPrint}${
        if readOnly then "val" else "var"
      } $name${typeRef.map(typeRef => s": ${typeRef.prettyPrint}").getOrElse("")}${initExpr.map(expr => s" = ${expr.prettyPrint}").getOrElse("")};"
}

case class TypeParameterDecl(
    name: String,
    variance: Variance,
    upperBounds: List[TypeRef],
    lowerBounds: List[TypeRef],
    range: SourceRange = SourceRange.dummy,
    modifiers: List[ModifierNode] = List.empty
) extends Decl {
  override def prettyPrint: String = s"${variance.prettyPrint}$name"
}

case class FunctionDecl(
    name: String,
    returnType: TypeRef,
    parameters: List[VariableDecl],
    block: BlockExpr,
    modifiers: List[ModifierNode] = List.empty,
    typeParameters: List[TypeParameterDecl] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends Decl {
  override def prettyPrint: String =
    s"${modifiers.prettyPrint}def $name${parameters.map(_.prettyPrintParam).mkString("(", ", ", ")}")}: ${returnType.prettyPrint} ${block.prettyPrint}"
}

sealed trait ClassLikeDecl(
    val name: String,
    val modifiers: List[ModifierNode] = List.empty,
    val superClasses: List[TypeRef] = List.empty,
    val fields: List[FieldDecl] = List.empty,
    val methods: List[FunctionDecl] = List.empty,
    val nestedClasses: List[ClassDecl] = List.empty,
    val nestedObjects: List[ObjectDecl] = List.empty,
    val range: SourceRange = SourceRange.dummy
) extends Decl

case class ClassDecl(
    override val name: String,
    override val modifiers: List[ModifierNode] = List.empty,
    override val superClasses: List[TypeRef] = List.empty,
    override val fields: List[FieldDecl] = List.empty,
    override val methods: List[FunctionDecl] = List.empty,
    override val nestedClasses: List[ClassDecl] = List.empty,
    override val nestedObjects: List[ObjectDecl] = List.empty,
    typeParameters: List[TypeParameterDecl] = List.empty,
    override val range: SourceRange = SourceRange.dummy
) extends ClassLikeDecl(name, modifiers, superClasses, fields, methods, nestedClasses, nestedObjects, range) {
  override def prettyPrint: String =
    s"${modifiers.prettyPrint}class $name${typeParameters.prettyPrint}${
        if superClasses.isEmpty then ""
        else s": ${superClasses.map(_.prettyPrint).mkString(", ")}"
      } {\n${fields.map(_.prettyPrint).mkString("\n")}\n${methods.map(_.prettyPrint).mkString("\n")}\n${nestedClasses.map(_.prettyPrint).mkString("\n")}\n${nestedObjects.map(_.prettyPrint).mkString("\n")}\n}"
}

case class ObjectDecl(
    override val name: String,
    override val modifiers: List[ModifierNode] = List.empty,
    override val superClasses: List[TypeRef] = List.empty,
    override val fields: List[FieldDecl] = List.empty,
    override val methods: List[FunctionDecl] = List.empty,
    override val nestedClasses: List[ClassDecl] = List.empty,
    override val nestedObjects: List[ObjectDecl] = List.empty,
    override val range: SourceRange = SourceRange.dummy
) extends ClassLikeDecl(name, modifiers, superClasses, fields, methods, nestedClasses, nestedObjects, range) {
  override def prettyPrint: String =
    s"${modifiers.prettyPrint}object $name${
        if superClasses.isEmpty then ""
        else s": ${superClasses.map(_.prettyPrint).mkString(", ")}"
      } {\n${fields.map(_.prettyPrint).mkString("\n")}\n${methods.map(_.prettyPrint).mkString("\n")}\n${nestedClasses.map(_.prettyPrint).mkString("\n")}\n${nestedObjects.map(_.prettyPrint).mkString("\n")}\n}"
}

case class KahwaFile(
    typedefDecls: List[TypedefDecl] = List.empty,
    classDecls: List[ClassDecl] = List.empty,
    objectDecls: List[ObjectDecl] = List.empty,
    functionDecls: List[FunctionDecl] = List.empty,
    variableDecls: List[VariableDecl] = List.empty,
    range: SourceRange = SourceRange.dummy
) extends Decl {
  override def prettyPrint: String =
    s"${typedefDecls.map(_.prettyPrint).mkString("\n")}\n${classDecls.map(_.prettyPrint).mkString("\n")}\n${objectDecls.map(_.prettyPrint).mkString("\n")}\n${functionDecls.map(_.prettyPrint).mkString("\n")}\n${variableDecls.map(_.prettyPrint).mkString("\n")}"

  override val name: String = "dummy"
  override val modifiers: List[ModifierNode] = List.empty
}
