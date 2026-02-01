package symbols

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import symbols.TypeSymbol

type SymbolTable[T <: Symbol] = mutable.Map[String, mutable.ListBuffer[T]]

class Scope {
  def searchCurrentForType(name: String): List[TypeSymbol] = {
    typeSymbolTable.getOrElse(name, Nil).toList
  }

  def searchCurrentForTerm(name: String): List[TermSymbol] = {
    termSymbolTable.getOrElse(name, Nil).toList
  }

  def searchForType(name: String): List[TypeSymbol] = {
    typeSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForType(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def searchForTerm(name: String): List[TermSymbol] = {
    termSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForTerm(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def define(symbol: Symbol): Unit = {
    symbol match {
      case symbol: TypeSymbol => typeSymbolTable.getOrElse(symbol.name, ListBuffer.empty) += symbol
      case symbol: TermSymbol => termSymbolTable.getOrElse(symbol.name, ListBuffer.empty) += symbol
      case unit: TranslationUnit => ???
    }
  }

  def defineAll(symbols: List[Symbol]): Unit = {
    symbols.foreach(define)
  }

  def addOuterScope(outerScope: Scope): Unit = {
    outerScopes += outerScope
  }

  private val typeSymbolTable: SymbolTable[TypeSymbol] = mutable.Map.empty
  private val termSymbolTable: SymbolTable[TermSymbol] = mutable.Map.empty

  private val outerScopes: mutable.ListBuffer[Scope] = mutable.ListBuffer.empty
}

object GlobalScope extends Scope {
  val ErrorType: SemanticType = SemanticType(TypeSymbol("Error-Type", GlobalScope), List.empty)
}