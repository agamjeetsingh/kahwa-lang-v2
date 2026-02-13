package symbols.analyser

import ast.Variance.{CONTRAVARIANT, COVARIANT}
import symbols.{ClassSymbol, Scope, SemanticType, TypeParameterSymbol, TypeSymbol}

object KahwaLangScope extends Scope {
  val ErrorTypeSymbol: TypeSymbol = TypeSymbol("Error-Type", KahwaLangScope)
  val ErrorType: SemanticType = SemanticType(ErrorTypeSymbol)
  
  val NothingSymbol: TypeSymbol = TypeSymbol("Nothing", KahwaLangScope)
  val NothingType: SemanticType = SemanticType(NothingSymbol)
  
  val AnySymbol: TypeSymbol = TypeSymbol("Any", KahwaLangScope)
  val AnyType: SemanticType = SemanticType(AnySymbol)

  val IntTypeSymbol: TypeSymbol = TypeSymbol("Int", KahwaLangScope)
  val IntType: SemanticType = SemanticType(IntTypeSymbol)
  
  val BoolTypeSymbol: TypeSymbol = TypeSymbol("Bool", KahwaLangScope)
  val BoolType: SemanticType = SemanticType(BoolTypeSymbol)
  
  val FloatTypeSymbol: TypeSymbol = TypeSymbol("Float", KahwaLangScope)
  val FloatType: SemanticType = SemanticType(FloatTypeSymbol)
  
  val StringTypeSymbol: TypeSymbol = TypeSymbol("String", KahwaLangScope)
  val StringType: SemanticType = SemanticType(StringTypeSymbol)
  
  val MIN_TUPLE_SIZE = 2
  val MAX_TUPLE_SIZE = 32
  val tupleSymbols: List[TypeSymbol] = (MIN_TUPLE_SIZE to MAX_TUPLE_SIZE).map(instantiateTupleX).toList
  
  val MIN_FUNCTION_SIZE = 0
  val MAX_FUNCTION_SIZE = 32
  val functionSymbols: List[TypeSymbol] = (MIN_FUNCTION_SIZE to MAX_FUNCTION_SIZE).map(instantiateFunctionX).toList
  
  private def instantiateTupleX(x: Int): ClassSymbol = {
    require(x >= MIN_TUPLE_SIZE && x <= MAX_TUPLE_SIZE)
    val classSymbol = ClassSymbol(s"Tuple${x.toString}", KahwaLangScope)
    classSymbol.genericArguments ++= (1 to x).map(n => TypeParameterSymbol(s"T$n", classSymbol.scope, COVARIANT))
    classSymbol
  }
  
  private def instantiateFunctionX(x: Int): ClassSymbol = {
    require(x >= MIN_FUNCTION_SIZE && x <= MAX_FUNCTION_SIZE)
    val classSymbol = ClassSymbol(s"Tuple${x.toString}", KahwaLangScope)
    classSymbol.genericArguments ++= (1 to x).map(n => TypeParameterSymbol(s"T$n", classSymbol.scope, CONTRAVARIANT))
    classSymbol.genericArguments += TypeParameterSymbol("R", classSymbol.scope, COVARIANT)
    classSymbol
  }
}
