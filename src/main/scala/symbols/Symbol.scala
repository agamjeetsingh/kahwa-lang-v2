package symbols

import ast.Modifier.PUBLIC
import ast.{Expr, Modifier, Variance}
import ast.Variance.INVARIANT

import scala.collection.mutable.ListBuffer

sealed abstract class Symbol(val name: String, outerScope: Scope) {
  val scope: Scope = {
    val s = Scope()
    s.addOuterScope(outerScope)
    s
  }
}

class TypeSymbol(name: String, scope: Scope) extends Symbol(name, scope)

class TypeParameterSymbol(override val name: String, outerScope: Scope, variance: Variance = INVARIANT) extends TypeSymbol(name, outerScope)

class SemanticType(val typeSymbol: TypeSymbol, val genericArguments: List[SemanticType])

class ClassSymbol(override val name: String, outerScope: Scope) extends TypeSymbol(name, outerScope) {
  val isAbstract: Boolean = false
  val isOpen: Boolean = false
  val visibility: Modifier = PUBLIC
  val genericArguments: ListBuffer[TypeParameterSymbol] = ListBuffer.empty
  val superClasses: ListBuffer[SemanticType] = ListBuffer.empty
  val methods: ListBuffer[?] = ListBuffer.empty
  val fields: ListBuffer[?] = ListBuffer.empty
  val nestedClasses: ListBuffer[ClassSymbol] = ListBuffer.empty
}

class VariableSymbol(override val name: String, outerScope: Scope, val semanticType: SemanticType, val expr: Expr) extends Symbol(name, outerScope) {
  val isStatic: Boolean = false
}

