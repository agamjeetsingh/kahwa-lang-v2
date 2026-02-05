package symbols

import ast.{BlockStmt, Expr, Modifier, TypeRef, Variance}
import ast.Variance.INVARIANT

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

sealed class TypeSymbol(name: String, scope: Scope) extends Symbol(name, scope)

sealed abstract class TermSymbol(name: String, scope: Scope) extends Symbol(name, scope)

class TypeParameterSymbol(override val name: String, outerScope: Scope, variance: Variance = INVARIANT) extends TypeSymbol(name, outerScope)

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
}

class VariableSymbol(override val name: String, outerScope: Scope) extends TermSymbol(name, outerScope) {
  var isStatic: Boolean = false
  var semanticType: SemanticType = GlobalScope.ErrorType
  val initExpr: Option[Expr] = None
}

class VisibleVariableSymbol(override val name: String, outerScope: Scope) extends VariableSymbol(name, outerScope) {
  var visibility: Visibility = Visibility.default
}

class FieldSymbol(override val name: String, outerScope: Scope) extends VisibleVariableSymbol(name, outerScope), Modal {
  var isAnOverride: Boolean = false
}

class FunctionSymbol(override val name: String, outerScope: Scope) extends TermSymbol(name, outerScope) {
  var isStatic: Boolean = false
  var visibility: Visibility = Visibility.default

  var block: BlockStmt = BlockStmt(List.empty)

  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val parameters: ListBuffer[VariableSymbol] = ListBuffer.empty

  var returnType: SemanticType = GlobalScope.ErrorType
}

class MethodSymbol(override val name: String, outerScope: Scope) extends FunctionSymbol(name, outerScope), Modal {
  var isAnOverride: Boolean = false
}

class TranslationUnit(override val name: String, outerScopes: List[Scope]) extends Symbol(name, outerScopes) {
  val classes: ListBuffer[ClassSymbol] = ListBuffer.empty
  val functions: ListBuffer[FunctionSymbol] = ListBuffer.empty
  val variables: ListBuffer[VisibleVariableSymbol] = ListBuffer.empty
  val typedefs: ListBuffer[TypedefSymbol] = ListBuffer.empty
}

class TypedefSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope) {
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  var referredType: SemanticType = GlobalScope.ErrorType
  var visibility: Visibility = Visibility.default
}

case class SemanticType(typeSymbol: TypeSymbol, genericArguments: List[SemanticType] = List.empty)