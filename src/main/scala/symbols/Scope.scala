package symbols

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type SymbolTable = mutable.Map[String, mutable.ListBuffer[Symbol]]

class Scope {
  def searchCurrent(name: String): List[Symbol] = {
    table.getOrElse(name, Nil).toList
  }

  def search(name: String): List[Symbol] = {
    table.getOrElse(name, outerScopes.iterator
      .map(_.search(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def define(symbol: Symbol): Unit = {
    table.getOrElse(symbol.name, ListBuffer.empty) += symbol
  }

  def defineAll(symbols: List[Symbol]): Unit = {
    symbols.foreach(define)
  }

  def addOuterScope(outerScope: Scope): Unit = {
    outerScopes += outerScope
  }

  private val table: SymbolTable = mutable.Map.empty

  private val outerScopes: mutable.ListBuffer[Scope] = mutable.ListBuffer.empty
}
