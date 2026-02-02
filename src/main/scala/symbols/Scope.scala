package symbols

import ast.Ident

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

  def searchForType(ident: Ident): List[TypeSymbol] = {
    // a.b.c
    def rec(head: String, tail: List[String]): List[TypeSymbol] = {
      // head = a; tail = List(b, c)
      tail match {
        case nextHead :: nextTail => {
          typeSymbolTable.getOrElse(head, List.empty).filter {
            case _: ClassSymbol => true
            case _ => false
          } match {
            case Nil => searchInParent() // a doesn't exist in current scope
            case h :: _ => h.scope.searchForType(Ident(nextHead, nextTail, ident.range)) match {
              case Nil => searchInParent() // (b.c) didn't get resolved correctly
              case res => res // Full a.b.c got resolved
            }
          }
        }
        case Nil => typeSymbolTable.getOrElse(head, searchInParent()).toList // There is no tail (just a), check directly
      }
    }

    def searchInParent(): List[TypeSymbol] = {
      outerScopes.iterator.map(_.searchForType(ident))
        .find(_.nonEmpty)
        .getOrElse(Nil)
    }

    ident match {
      case Ident(head, tail, _) => rec(head, tail)
    }
  }

  def searchForType(name: String): List[TypeSymbol] = {
    typeSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForType(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def searchForTerm(ident: Ident): List[TermSymbol] = {
    // a.b.c
    def rec(head: String, tail: List[String]): List[TermSymbol] = {
      // head = a; tail = List(b, c)
      tail match {
        case nextHead :: nextTail => {
          typeSymbolTable.getOrElse(head, List.empty).filter {
            case _: ClassSymbol => true
            case _ => false
          } match {
            case Nil => searchInParent() // a doesn't exist in current scope
            case h :: _ => h.scope.searchForTerm(Ident(nextHead, nextTail, ident.range)) match {
              case Nil => searchInParent() // (b.c) didn't get resolved correctly
              case res => res // Full a.b.c got resolved
            }
          }
        }
        case Nil => termSymbolTable.getOrElse(head, searchInParent()).toList // There is no tail (just a), check directly
      }
    }

    def searchInParent(): List[TermSymbol] = {
      outerScopes.iterator.map(_.searchForTerm(ident))
        .find(_.nonEmpty)
        .getOrElse(Nil)
    }

    ident match {
      case Ident(head, tail, _) => rec(head, tail)
    }
  }

  def searchForTerm(name: String): List[TermSymbol] = {
    termSymbolTable.getOrElse(name, outerScopes.iterator
      .map(_.searchForTerm(name))
      .find(_.nonEmpty)
      .getOrElse(Nil)).toList
  }

  def define(symbol: Symbol): Unit = {
    symbol match {
      case symbol: TypeSymbol => typeSymbolTable.getOrElseUpdate(symbol.name, ListBuffer.empty) += symbol
      case symbol: TermSymbol => termSymbolTable.getOrElseUpdate(symbol.name, ListBuffer.empty) += symbol
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