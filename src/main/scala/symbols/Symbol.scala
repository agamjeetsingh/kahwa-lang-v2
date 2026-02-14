package symbols

import ast.{BlockExpr, Modifier, PrettyPrintable, TypeRef, Variance}
import ast.Variance.INVARIANT
import symbols.analyser.KahwaLangScope

import scala.collection.mutable.ListBuffer

sealed abstract class Symbol(val name: String, outerScopes: List[Scope]) {
  def this(name: String, outerScope: Scope) = this(name, List(outerScope))

  val scope: Scope = {
    val s = Scope()
    outerScopes.foreach(s.addOuterScope)
    s
  }

  def isType: Boolean = this match {
    case _: TypeSymbol => true
    case _ => false
  }

  def isTerm: Boolean = this match {
    case symbol: TypeSymbol => false
    case _: VariableSymbol | _: FunctionSymbol => true
    case _: symbols.TranslationUnit => ???
  }
}

sealed class TypeSymbol(name: String, scope: Scope) extends Symbol(name, scope), PrettyPrintable {
  override def prettyPrint: String = name
}

sealed abstract class TermSymbol(name: String, scope: Scope) extends Symbol(name, scope)

class TypeParameterSymbol(
    override val name: String,
    outerScope: Scope,
    variance: Variance = INVARIANT
) extends TypeSymbol(name, outerScope)

sealed trait Modal {
  var isAbstract: Boolean = false
  var isOpen: Boolean = false

  def setModality(modifier: Modifier): Unit = {
    modifier match {
      case Modifier.OPEN => isAbstract = false; isOpen = true
      case Modifier.FINAL => isAbstract = false; isOpen = false
      case Modifier.ABSTRACT => isAbstract = true; isOpen = true
      case _ =>
    }
  }
}

class ClassSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope), Modal {
  var visibility: Visibility = Visibility.default
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty
  val methods: ListBuffer[MethodSymbol] = ListBuffer.empty
  val fields: ListBuffer[FieldSymbol] = ListBuffer.empty
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty
  val nestedObjects: ListBuffer[ObjectSymbol] = ListBuffer.empty
  val linkedObject: Option[ObjectSymbol] = None
}

class ObjectSymbol(override val name: String, outerScope: Scope) extends TermSymbol(name, outerScope), Modal {
  var visibility: Visibility = Visibility.default
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty
  val methods: ListBuffer[MethodSymbol] = ListBuffer.empty
  val fields: ListBuffer[FieldSymbol] = ListBuffer.empty
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty
  val nestedObjects: ListBuffer[ObjectSymbol] = ListBuffer.empty
  val linkedClass: Option[ClassSymbol] = None
}

class VariableSymbol(override val name: String, outerScope: Scope) extends TermSymbol(name, outerScope) {
  var semanticType: SemanticType = KahwaLangScope.ErrorType
  val initExpr: Option[BoundExpr] = None
}

class VisibleVariableSymbol(override val name: String, outerScope: Scope) extends VariableSymbol(name, outerScope) {
  var visibility: Visibility = Visibility.default
}

class FieldSymbol(override val name: String, outerScope: Scope) extends VisibleVariableSymbol(name, outerScope), Modal {
  var isAnOverride: Boolean = false
}

class FunctionSymbol(override val name: String, outerScope: Scope) extends TermSymbol(name, outerScope) {
  var visibility: Visibility = Visibility.default

  var block: BoundBlockExpr = BoundBlockExpr(List.empty, KahwaLangScope.NothingType)

  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val parameters: ListBuffer[VariableSymbol] = ListBuffer.empty

  var returnType: SemanticType = KahwaLangScope.ErrorType
}

class MethodSymbol(override val name: String, outerScope: Scope) extends FunctionSymbol(name, outerScope), Modal {
  var isAnOverride: Boolean = false
}

class TranslationUnit(override val name: String, outerScopes: List[Scope]) extends Symbol(name, outerScopes) {
  val classes: ListBuffer[ClassSymbol] = ListBuffer.empty
  val objects: ListBuffer[ObjectSymbol] = ListBuffer.empty
  val functions: ListBuffer[FunctionSymbol] = ListBuffer.empty
  val variables: ListBuffer[VisibleVariableSymbol] = ListBuffer.empty
  val typedefs: ListBuffer[TypedefSymbol] = ListBuffer.empty
}

class TypedefSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope) {
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  var referredType: SemanticType = KahwaLangScope.ErrorType
  var visibility: Visibility = Visibility.default
}

case class SemanticType(
    typeSymbol: TypeSymbol,
    genericArguments: List[SemanticType] = List.empty
) extends PrettyPrintable {
  override def prettyPrint: String =
    s"${typeSymbol.prettyPrint}${genericArguments.map(_.prettyPrint).mkString("[", ", ", "]")}"
}

object SemanticType {
  extension (t1: SemanticType) {
    infix def <(t2: SemanticType): Boolean = {
      if (t2 == KahwaLangScope.AnyType || t2 == KahwaLangScope.ErrorType) return true
      if (t1 == KahwaLangScope.NothingType || t2 == KahwaLangScope.NothingType) return true
      (t1, t2) match {
        case (SemanticType(typeSymbol1, genericArguments1), SemanticType(typeSymbol2, genericArguments2)) =>
          if (typeSymbol1 == typeSymbol2) {
            ???
          } else {
            ???
          }
      }
    }
  }
}

sealed trait BoundExpr {
  def semanticType: SemanticType
}

sealed trait BoundLiteralExpr extends BoundExpr

case class BoundBoolLiteral(value: Boolean) extends BoundLiteralExpr {
  override def semanticType: SemanticType = KahwaLangScope.BoolType
}

case class BoundFloatLiteral(value: Float) extends BoundLiteralExpr {
  override def semanticType: SemanticType = KahwaLangScope.FloatType
}

case class BoundIntegerLiteral(value: Int) extends BoundLiteralExpr {
  override def semanticType: SemanticType = KahwaLangScope.IntType
}

case class BoundStringLiteral(value: String) extends BoundLiteralExpr {
  override def semanticType: SemanticType = KahwaLangScope.StringType
}

case class BoundVariable(variableSymbol: VariableSymbol, override val semanticType: SemanticType) extends BoundExpr

case class FunctionCall(
    functionSymbol: FunctionSymbol,
    genericArguments: List[SemanticType],
    args: BoundExpr,
    override val semanticType: SemanticType
) extends BoundExpr {
  require(genericArguments.size == functionSymbol.genericArguments.size)
}

case class MethodCall(
    methodSymbol: MethodSymbol,
    target: BoundExpr,
    genericArguments: List[SemanticType],
    args: BoundExpr,
    override val semanticType: SemanticType
) extends BoundExpr {
  require(genericArguments.size == methodSymbol.genericArguments.size)
}

case class FieldAccess(fieldSymbol: FieldSymbol, target: BoundExpr, override val semanticType: SemanticType)
    extends BoundExpr

case class BoundBlockExpr(exprs: List[BoundExpr], override val semanticType: SemanticType) extends BoundExpr

case class IfExpr(
    expr: BoundExpr,
    ifBlock: BoundBlockExpr,
    elseBlock: Option[BoundBlockExpr] = None,
    override val semanticType: SemanticType
) extends BoundExpr

case class BoundWhileExpr(
    cond: BoundExpr,
    body: BoundBlockExpr,
    override val semanticType: SemanticType
) extends BoundExpr

case class BoundBreak() extends BoundExpr {
  override def semanticType: SemanticType = KahwaLangScope.NothingType
}

case class BoundContinue() extends BoundExpr {
  override def semanticType: SemanticType = KahwaLangScope.NothingType
}

case class BoundVariableDecl(
    name: String,
    varType: SemanticType,
    readOnly: Boolean,
    initExpr: Option[BoundExpr] = None,
) extends BoundExpr {
  override def semanticType: SemanticType = KahwaLangScope.UnitType
}
