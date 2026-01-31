package symbols

import ast.{BlockStmt, Expr, Variance}
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
    case symbol: TypeSymbol => true
    case symbol: VariableSymbol => false
  }

  def isTerm: Boolean = this match {
    case symbol: TypeSymbol => false
    case symbol: VariableSymbol => true
  }
}

class TypeSymbol(name: String, scope: Scope) extends Symbol(name, scope)

class TypeParameterSymbol(override val name: String, outerScope: Scope, variance: Variance = INVARIANT) extends TypeSymbol(name, outerScope)

class SemanticType(val typeSymbol: TypeSymbol, val genericArguments: List[SemanticType])

class ClassSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope) {
  val isAbstract: Boolean = false
  val isOpen: Boolean = false
  val visibility: Visibility = Visibility.default
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty
  val methods: ListBuffer[MethodSymbol] = ListBuffer.empty
  val fields: ListBuffer[FieldSymbol] = ListBuffer.empty
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty
}

class VariableSymbol(override val name: String, outerScope: Scope, val semanticType: SemanticType, val initExpr: Option[Expr]) extends Symbol(name, outerScope) {
  val isStatic: Boolean = false
}

class VisibleVariableSymbol(override val name: String, outerScope: Scope, override val semanticType: SemanticType, override val initExpr: Option[Expr]) extends VariableSymbol(name, outerScope, semanticType, initExpr) {
  val visibility: Visibility = Visibility.default
}

class FieldSymbol(override val name: String, outerScope: Scope, override val semanticType: SemanticType, override val initExpr: Option[Expr]) extends VisibleVariableSymbol(name, outerScope, semanticType, initExpr) {
  val isAbstract: Boolean = false
  val isOpen: Boolean = false
  val isAnOverride: Boolean = false
}

class FunctionSymbol(override val name: String, outerScope: Scope) extends Symbol(name, outerScope) {
  val isStatic: Boolean = false
  val visibility: Visibility = Visibility.default

  val block: BlockStmt = BlockStmt(List.empty)

  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val parameters: ListBuffer[VariableSymbol] = ListBuffer.empty

  val returnType: SemanticType = GlobalScope.NothingType
}

class MethodSymbol(override val name: String, outerScope: Scope) extends FunctionSymbol(name, outerScope) {
  val isAbstract: Boolean = false
  val isOpen: Boolean = false
  val isAnOverride: Boolean = false
}

class TranslationUnit(override val name: String, outerScopes: List[Scope]) extends Symbol(name, outerScopes) {
  val classes: ListBuffer[ClassSymbol] = ListBuffer.empty
  val functions: ListBuffer[FunctionSymbol] = ListBuffer.empty
  val variables: ListBuffer[VisibleVariableSymbol] = ListBuffer.empty
}