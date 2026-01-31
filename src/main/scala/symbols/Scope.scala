package symbols

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type SymbolTable = mutable.Map[String, mutable.ListBuffer[Symbol]]

class Scope {
  def searchCurrentForType(name: String): List[Symbol] = {
    typeSymbolTable.getOrElse(name, Nil).toList
  }

  def searchCurrentForTerm(name: String): List[Symbol] = {
    termSymbolTable.getOrElse(name, Nil).toList
  }

  def searchForType(name: String): List[Symbol] = {
    typeSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForType(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def searchForTerm(name: String): List[Symbol] = {
    typeSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForTerm(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def define(symbol: Symbol): Unit = {
    if (symbol.isType) typeSymbolTable.getOrElse(symbol.name, ListBuffer.empty) += symbol
    else if (symbol.isTerm) termSymbolTable.getOrElse(symbol.name, ListBuffer.empty) += symbol
  }

  def defineAll(symbols: List[Symbol]): Unit = {
    symbols.foreach(define)
  }

  def addOuterScope(outerScope: Scope): Unit = {
    outerScopes += outerScope
  }

  private val typeSymbolTable: SymbolTable = mutable.Map.empty
  private val termSymbolTable: SymbolTable = mutable.Map.empty

  private val outerScopes: mutable.ListBuffer[Scope] = mutable.ListBuffer.empty
}

object GlobalScope extends Scope {
  val NothingType: SemanticType = SemanticType(TypeSymbol("Nothing", GlobalScope), List.empty)
}