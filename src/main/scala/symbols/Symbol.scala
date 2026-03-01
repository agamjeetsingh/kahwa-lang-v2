package symbols

import ast.{Modifier, PrettyPrintable, Variance}
import ast.Variance.INVARIANT
import symbols.analyser.KahwaLangScope
import symbols.analyser.TypeRefQualifier
import symbols.analyser.DeclareNames
import symbols.analyser.TypeChecker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed abstract class Symbol(val name: String, outerScopes: List[Scope]) {
  def this(name: String, outerScope: Scope) = this(name, List(outerScope))

  val scope: Scope = {
    val s = Scope()
    outerScopes.foreach(s.addOuterScope)
    s
  }
}

sealed class TypeSymbol(name: String, scope: Scope) extends Symbol(name, scope), PrettyPrintable {
  override def prettyPrint: String = name
}

sealed abstract class TermSymbol(name: String, scope: Scope) extends Symbol(name, scope)

sealed abstract class OverloadableTermSymbol(name: String, scope: Scope) extends TermSymbol(name, scope)

sealed abstract class NonOverloadableTermSymbol(name: String, scope: Scope) extends TermSymbol(name, scope)

/**
 * Initialised completely by [[DeclareNames]]
 */
class TypeParameterSymbol(
    override val name: String,
    outerScope: Scope,
    val variance: Variance
) extends TypeSymbol(name, outerScope) {

  /**
   * Initialised completely by [[TypeRefQualifier]]
   */
  val upperBounds: ListBuffer[SemanticType] = ListBuffer.empty

  /**
   * Initialised completely by [[TypeRefQualifier]]
   */
  val lowerBounds: ListBuffer[SemanticType] = ListBuffer.empty
}

/**
 * Initialised completely by [[DeclareNames]]
 */
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

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var visibility: Visibility = Visibility.default

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[TypeRefQualifier]]
   */
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val methods: ListBuffer[MethodSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val fields: ListBuffer[FieldSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val nestedObjects: ListBuffer[ObjectSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   * @todo TODO - Hasn't been done yet
   */
  val linkedObject: Option[ObjectSymbol] = None

  def substitute(toBeSubstituted: SemanticType, concreteArgs: List[SemanticType]): SemanticType =
    require(concreteArgs.size == genericArguments.size)
    ClassSymbol.substitute(toBeSubstituted, genericArguments.zip(concreteArgs).toMap)
}

object ClassSymbol {
  private def substitute(toBeSubstituted: SemanticType, blueprint: Map[TypeParameterSymbol, SemanticType]): SemanticType = {
    toBeSubstituted match {
      case SemanticType(typeSymbol, genericArguments) =>
        if (toBeSubstituted.genericArguments.isEmpty) {
          Some(typeSymbol).collect { case s: TypeParameterSymbol => s }
            .flatMap(blueprint.get)
            .getOrElse(toBeSubstituted)

        } else {
          SemanticType(typeSymbol, genericArguments.map(substitute(_, blueprint)))
        }
    }
  }
}

class ObjectSymbol(override val name: String, outerScope: Scope)
    extends NonOverloadableTermSymbol(name, outerScope),
      Modal {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var visibility: Visibility = Visibility.default

  /**
   * Initialised completely by [[TypeRefQualifier]]
   */
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val methods: ListBuffer[MethodSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val fields: ListBuffer[FieldSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val nestedObjects: ListBuffer[ObjectSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   * @todo TODO - Hasn't been done yet
   */
  val linkedClass: Option[ClassSymbol] = None
}

class VariableSymbol(override val name: String, outerScope: Scope) extends NonOverloadableTermSymbol(name, outerScope) {

  /**
   * Initialised properly by [[TypeRefQualifier]]
   */
  var semanticType: SemanticType = KahwaLangScope.ErrorType

  /**
   * Initialised properly by [[TypeChecker]]
   */
  var initExpr: Option[BoundExpr] = None
}

class VisibleVariableSymbol(override val name: String, outerScope: Scope) extends VariableSymbol(name, outerScope) {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var visibility: Visibility = Visibility.default
}

class FieldSymbol(override val name: String, outerScope: Scope) extends VisibleVariableSymbol(name, outerScope), Modal {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var isAnOverride: Boolean = false
}

class FunctionSymbol(override val name: String, outerScope: Scope) extends OverloadableTermSymbol(name, outerScope) {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var visibility: Visibility = Visibility.default

  /**
   * Initialised properly by [[TypeChecker]]
   */
  var block: BoundBlockExpr = BoundBlockExpr(List.empty, KahwaLangScope.NothingType, Scope())

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val parameters: ListBuffer[VariableSymbol] = ListBuffer.empty

  /**
   * Initialised properly by [[TypeChecker]]
   */
  var returnType: SemanticType = KahwaLangScope.ErrorType
}

class MethodSymbol(override val name: String, outerScope: Scope) extends FunctionSymbol(name, outerScope), Modal {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var isAnOverride: Boolean = false
}

/**
 * @todo outerScopes currently set by [[DeclareNames]] as an empty list because imports have not been implemented
 */
class TranslationUnit(
    override val name: String,
    outerScopes: List[Scope]
) extends Symbol(name, outerScopes) {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val classes: ListBuffer[ClassSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val objects: ListBuffer[ObjectSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val functions: ListBuffer[FunctionSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val variables: ListBuffer[VisibleVariableSymbol] = ListBuffer.empty

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val typedefs: ListBuffer[TypedefSymbol] = ListBuffer.empty
}

class TypedefSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope) {

  /**
   * Initialised completely by [[DeclareNames]]
   */
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty

  /**
   * Initialised properly by [[TypeRefQualifier]]
   */
  var referredType: SemanticType = KahwaLangScope.ErrorType

  /**
   * Initialised completely by [[DeclareNames]]
   */
  var visibility: Visibility = Visibility.default
}

case class SemanticType(
    typeSymbol: TypeSymbol,
    genericArguments: List[SemanticType] = List.empty
) extends PrettyPrintable {
  override def prettyPrint: String =
    s"${typeSymbol.prettyPrint}${
        if (genericArguments.isEmpty) "" else genericArguments.map(_.prettyPrint).mkString("[", ", ", "]")
      }"
}

object SemanticType {
  extension (t1: SemanticType) {
    infix def subtypeOf(t2: SemanticType): Boolean = {
      if (t1 == KahwaLangScope.ErrorType || t2 == KahwaLangScope.ErrorType) return true
      if (t1 == KahwaLangScope.NothingType || t2 == KahwaLangScope.AnyType) return true

      (t1, t2) match {
        case (SemanticType(typeSymbol1, genericArguments1), SemanticType(typeSymbol2, genericArguments2)) =>
          (typeSymbol1, typeSymbol2) match {
            case (typeParameterSymbol: TypeParameterSymbol, _) =>
              typeParameterSymbol.upperBounds.exists(_.subtypeOf(t2))
            case (_, typeParameterSymbol: TypeParameterSymbol) =>
              typeParameterSymbol.lowerBounds.exists(t1.subtypeOf)
            case (classSymbol1: ClassSymbol, classSymbol2: ClassSymbol) => {
              // TODO - Not sure if the == uniquely identifies it or not
              if (classSymbol1 == classSymbol2) {
                require(genericArguments1.size == genericArguments2.size)
                genericArguments1.zip(genericArguments2).zip(classSymbol1.genericArguments.map(_.variance)).forall {
                  tuple =>
                    val ((a, b), variance) = tuple
                    variance match {
                      case Variance.COVARIANT => a.subtypeOf(b)
                      case Variance.CONTRAVARIANT => b.subtypeOf(a)
                      case Variance.INVARIANT => a.subtypeOf(b) && b.subtypeOf(a)
                    }
                }
              } else {
                classSymbol1.superClasses.exists(superClassType =>
                  classSymbol1.substitute(superClassType, t1.genericArguments).subtypeOf(t2)
                )
              }
            }
            case _ => {
              ???
            }
          }
      }
    }

    infix def typeUnion(t2: SemanticType): SemanticType = ???
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

object BoundVariable {
  val ErrorVariable = BoundVariable(VariableSymbol("error-variable-symbol", KahwaLangScope), KahwaLangScope.ErrorType)
}

case class FunctionCall(
    functionSymbol: FunctionSymbol,
    genericArguments: List[SemanticType],
    args: List[BoundExpr],
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

case class BoundBlockExpr(
    exprs: List[BoundExpr],
    override val semanticType: SemanticType,
    scope: Scope,
    vars: ListBuffer[VariableSymbol] = ListBuffer.empty
) extends BoundExpr

case class BoundIfExpr(
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
