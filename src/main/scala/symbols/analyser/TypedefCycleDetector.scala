package symbols.analyser

import ast.TypedefDecl
import diagnostics.Diagnostic
import diagnostics.Diagnostic.TypedefCycleDetected
import sources.SourceRange
import symbols.analyser.SemanticAnalyser.MutableNodeToSymbol

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private object TypedefCycleDetector {
  def detectCycles(allTypedefs: List[TypedefDecl])(using nodeToSymbol: MutableNodeToSymbol): List[Diagnostic] = {
    val visited = mutable.Set[String]()
    val visiting = mutable.Set[String]()
    val diagnostics = ListBuffer[Diagnostic]()

    def dfs(typedefName: String, path: List[String]): Unit = {
      if (visiting.contains(typedefName)) {
        diagnostics += TypedefCycleDetected(path.reverse, SourceRange.dummy) // TODO - Source range
        return
      }
      if (visited.contains(typedefName)) return

      visiting += typedefName
      // Get all typedef dependencies
      for (dep <- getTypedefDependencies(typedefName, allTypedefs)) {
        dfs(dep, typedefName :: path)
      }
      visiting -= typedefName
      visited += typedefName
    }

    allTypedefs.foreach(td => dfs(td.name, List(td.name)))
    diagnostics.toList
  }

  private def getTypedefDependencies(name: String, allTypedefs: List[TypedefDecl])(using nodeToSymbol: MutableNodeToSymbol): List[String] = {
    allTypedefs.collect {
      case typedefDecl if nodeToSymbol.contains(typedefDecl) => typedefDecl
    }.find(_.name == name) match {
      case Some(typedefDecl) => {
        val typeRef = typedefDecl.referredType
        // TODO
        typeRef.name.prettyPrint :: typeRef.args.map(_._1.name.prettyPrint)
      }
      case None => List.empty
    }
  }
}
