package symbols.analyser

import ast.Variance.{CONTRAVARIANT, COVARIANT}
import symbols.{ClassSymbol, Scope, SemanticType, TypeParameterSymbol, TypeSymbol}

object KahwaLangScope extends Scope {
  val ErrorTypeSymbol: TypeSymbol = TypeSymbol("Error-Type", KahwaLangScope)
  val ErrorType: SemanticType = SemanticType(ErrorTypeSymbol)

  define(ErrorTypeSymbol)
  
  val NothingSymbol: TypeSymbol = TypeSymbol("Nothing", KahwaLangScope)
  val NothingType: SemanticType = SemanticType(NothingSymbol)

  define(NothingSymbol)
  
  val AnySymbol: TypeSymbol = TypeSymbol("Any", KahwaLangScope)
  val AnyType: SemanticType = SemanticType(AnySymbol)

  define(AnySymbol)

  val IntTypeSymbol: TypeSymbol = TypeSymbol("Int", KahwaLangScope)
  val IntType: SemanticType = SemanticType(IntTypeSymbol)

  define(IntTypeSymbol)
  
  val BoolTypeSymbol: TypeSymbol = TypeSymbol("Bool", KahwaLangScope)
  val BoolType: SemanticType = SemanticType(BoolTypeSymbol)

  define(BoolTypeSymbol)
  
  val FloatTypeSymbol: TypeSymbol = TypeSymbol("Float", KahwaLangScope)
  val FloatType: SemanticType = SemanticType(FloatTypeSymbol)

  define(FloatTypeSymbol)
  
  val StringTypeSymbol: TypeSymbol = TypeSymbol("String", KahwaLangScope)
  val StringType: SemanticType = SemanticType(StringTypeSymbol)

  define(StringTypeSymbol)

  val UnitTypeSymbol: TypeSymbol = TypeSymbol("Unit", KahwaLangScope)
  val UnitType: SemanticType = SemanticType(UnitTypeSymbol)

  define(UnitTypeSymbol)
  
  val MIN_TUPLE_SIZE = 2
  val MAX_TUPLE_SIZE = 32
  val tupleSymbols: List[TypeSymbol] = (MIN_TUPLE_SIZE to MAX_TUPLE_SIZE).map(instantiateTupleX).toList

  tupleSymbols.foreach(define)
  
  val MIN_FUNCTION_SIZE = 0
  val MAX_FUNCTION_SIZE = 32
  val functionSymbols: List[TypeSymbol] = (MIN_FUNCTION_SIZE to MAX_FUNCTION_SIZE).map(instantiateFunctionX).toList

  functionSymbols.foreach(define)
  
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
