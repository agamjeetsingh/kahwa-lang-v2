package symbols.analyser

import ast.BinaryOp
import ast.Variance.{CONTRAVARIANT, COVARIANT}
import cats.data.NonEmptyList
import symbols.{ClassSymbol, FunctionSymbol, Scope, SemanticType, TypeParameterSymbol, TypeSymbol, VariableSymbol}

import scala.collection.mutable

object KahwaLangScope extends Scope {

  private def createInbuiltType(name: String): SemanticType = {
    val typeSymbol: TypeSymbol = ClassSymbol(name, KahwaLangScope)
    val semanticType: SemanticType = SemanticType(typeSymbol)
    semanticTypeToSymbol(semanticType) = typeSymbol
    define(typeSymbol)

    semanticType
  }
  
  private val semanticTypeToSymbol: mutable.Map[SemanticType, TypeSymbol] = mutable.Map.empty
  
  def getTypeSymbol(inbuiltType: SemanticType): TypeSymbol = semanticTypeToSymbol(inbuiltType)
  
  val ErrorType: SemanticType = createInbuiltType("Error-Type")
  
  val NothingType: SemanticType = createInbuiltType("Nothing")
  
  val AnyType: SemanticType = createInbuiltType("Any")
  
  val CharType: SemanticType = createInbuiltType("Char")
  val IntType: SemanticType = createInbuiltType("Int")
  val LongType: SemanticType = createInbuiltType("Long")
  val FloatType: SemanticType = createInbuiltType("Float")
  val DoubleType: SemanticType = createInbuiltType("Double")
  
  val BoolType: SemanticType = createInbuiltType("Bool")
  
  val StringType: SemanticType = createInbuiltType("String")
  
  val UnitType: SemanticType = createInbuiltType("Unit")
  
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
    val classSymbol = ClassSymbol(s"Function${x.toString}", KahwaLangScope)
    classSymbol.genericArguments ++= (1 to x).map(n => TypeParameterSymbol(s"T$n", classSymbol.scope, CONTRAVARIANT))
    classSymbol.genericArguments += TypeParameterSymbol("R", classSymbol.scope, COVARIANT)
    classSymbol
  }

  // Char -> Int -> Long -> Float -> Double
  // Int -> Char

  private def createNativeFunc(name: String, returnType: SemanticType, parameters: List[SemanticType]): FunctionSymbol = {
    val functionSymbol = FunctionSymbol(name, KahwaLangScope)
    functionSymbol.returnType = returnType
    functionSymbol.parameters ++= parameters.zipWithIndex.map((semanticType, i) => (semanticType, s"x$i")).map { (semanticType, name) =>
      val variableSymbol = VariableSymbol(name, functionSymbol.scope)
      variableSymbol.semanticType = semanticType
      variableSymbol
    }
    functionSymbol
  }

  private val implicitConversions: mutable.Map[(SemanticType, SemanticType), FunctionSymbol] = mutable.Map.empty

  private def createImplicitConvertor(from: SemanticType, to: SemanticType): Unit = {
    implicitConversions += (from, to) -> createNativeFunc(s"--${from.prettyPrint}-to-${to.prettyPrint}--", to, List(from))
  }

  List(
    CharType -> IntType,
    IntType -> LongType,
    LongType -> FloatType,
    FloatType -> DoubleType,
    IntType -> CharType,
    AnyType -> UnitType
  ).foreach(createImplicitConvertor)

  private def createBinaryOp(name: String, semanticType: IntType.type | FloatType.type, returnType: SemanticType, parameterType: SemanticType): FunctionSymbol = {
    createNativeFunc(s"--$name-${semanticType match {
      case IntType => "int"
      case FloatType => "float"
    }}--", returnType, List(parameterType, parameterType))
  }

  def getConversion(from: SemanticType, to: SemanticType): Option[NonEmptyList[FunctionSymbol]] = {
    if (from == to) {
      return None
    }

    implicitConversions.get((from, to)) match {
      case Some(conversion) => return Some(NonEmptyList.one(conversion))
      case None =>
    }
    
    val queue = mutable.Queue[(SemanticType, List[FunctionSymbol])]((from, List.empty))
    val visited = mutable.Set[SemanticType](from)

    while (queue.nonEmpty) {
      val (currentType, path) = queue.dequeue()

      for (((fromType, toType), conversion) <- implicitConversions if fromType == currentType) {
        if (toType == to) {
          val fullPath = path :+ conversion
          return Some(NonEmptyList.fromListUnsafe(fullPath))
        }

        if (!visited.contains(toType)) {
          visited.add(toType)
          queue.enqueue((toType, path :+ conversion))
        }
      }
    }

    None
  }

  // BinaryOp.EQUALS, =

  // BinaryOp.DOUBLE_EQUALS, ==

  // BinaryOp.NOT_EQUALS, !=



  // BinaryOp.LESS, <
  val lessInt: FunctionSymbol = createBinaryOp("less", IntType, BoolType, IntType)
  val lessFloat: FunctionSymbol = createNativeFunc("--less-float--", BoolType, List(FloatType, FloatType))

  // BinaryOp.GREATER, >
  val greaterInt: FunctionSymbol = createNativeFunc("--less-int--", BoolType, List(IntType, IntType))
  val greaterFloat: FunctionSymbol = createNativeFunc("--less-float--", BoolType, List(FloatType, FloatType))

  // BinaryOp.LESS_EQUALS, <=
  val lessEqualsInt: FunctionSymbol = createNativeFunc("--less-equals-int--", BoolType, List(IntType, IntType))


  val addInt: FunctionSymbol = createNativeFunc("--add-int--", IntType, List(IntType, IntType))
  val addFloat: FunctionSymbol = createNativeFunc("--add-float--", FloatType, List(FloatType, FloatType))


  var incrementInt: FunctionSymbol = FunctionSymbol("--increment-int--", KahwaLangScope)
}
