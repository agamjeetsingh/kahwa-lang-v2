package symbols.analyser

import ast.TypedefDecl
import diagnostics.Diagnostic
import diagnostics.Diagnostic.TypedefCycleDetected
import symbols.TypedefSymbol
import symbols.analyser.SemanticAnalyser.MutableNodeToSymbol

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private object TypedefCycleDetector {
  def detectCycles(
      allTypedefs: List[TypedefDecl]
  )(using nodeToSymbol: MutableNodeToSymbol): List[Diagnostic] = {
    val visited = mutable.Set[TypedefSymbol]()
    val visiting = mutable.Set[TypedefSymbol]()
    val diagnostics = ListBuffer[Diagnostic]()

    val typedefSymbolToNode = allTypedefs.collect { typedefDecl =>
      nodeToSymbol(typedefDecl) match {
        case typedefSymbol: TypedefSymbol => typedefSymbol -> typedefDecl
      }
    }.toMap

    def dfs(typedefSymbol: TypedefSymbol, path: List[TypedefSymbol]): Unit = {
      if (visiting.contains(typedefSymbol)) {
        diagnostics += TypedefCycleDetected(
          path.reverse.map(typedefSymbolToNode.apply).map(_.prettyPrint),
          typedefSymbolToNode(typedefSymbol).range
        )
        return
      }
      if (visited.contains(typedefSymbol)) return

      visiting += typedefSymbol
      for (dep <- getTypedefDependencies(typedefSymbol)) {
        dfs(dep, typedefSymbol :: path)
      }
      visiting -= typedefSymbol
      visited += typedefSymbol
    }

    allTypedefs.collect { typedefDecl =>
      nodeToSymbol(typedefDecl) match {
        case typedefSymbol: TypedefSymbol => typedefSymbol
      }
    }
      .foreach(td => dfs(td, List(td)))
    diagnostics.toList
  }

  private def getTypedefDependencies(
      typedefSymbol: TypedefSymbol
  )(using nodeToSymbol: MutableNodeToSymbol): List[TypedefSymbol] = {
    val semanticType = typedefSymbol.referredType
    (semanticType.typeSymbol match {
      case typedefSymbol: TypedefSymbol => List(typedefSymbol)
      case _ => List.empty
    }) ++ semanticType.genericArguments.collect { arg =>
      arg.typeSymbol match {
        case typedefSymbol: TypedefSymbol => typedefSymbol
      }
    }
      .flatMap(getTypedefDependencies)
  }
}
