package symbols

import ast.{ExprIdent, Ident}
import cats.data.NonEmptyList
import sources.SourceRange

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import symbols.TypeSymbol

class Scope {

  // ===== Searching for types =====
  def searchForType(exprIdent: ExprIdent): Option[TypeSymbol] = {
    searchForType(Ident(exprIdent.head, exprIdent.tail, exprIdent.range))
  }

  def searchForType(name: String): Option[TypeSymbol] = searchForType(Ident(name))

  def searchForType(ident: Ident): Option[TypeSymbol] = {
    // a.b.c
    def rec(head: String, tail: List[String]): Option[TypeSymbol] = {
      // head = a; tail = List(b, c)
      tail match {
        case nextHead :: nextTail =>
          typeSymbolTable.get(head) match {
            case Some(typeSymbol: ClassSymbol) =>
              typeSymbol.scope.searchForType(
                Ident(nextHead, nextTail, ident.range)
              ) match {
                case None => searchInParent() // (b.c) didn't get resolved correctly
                case res => res // Full a.b.c got resolved
              }
            case _ => searchInParent() // a doesn't exist in current scope or is not a class
          }
        case Nil =>
          typeSymbolTable.get(head) match {
            case None => searchInParent()
            case result => result
          }
      }
    }

    def searchInParent(): Option[TypeSymbol] = {
      outerScopes.iterator
        .map(_.searchForType(ident))
        .find(_.nonEmpty)
        .flatten
    }

    ident match {
      case Ident(head, tail, _) => rec(head, tail)
    }
  }

  // ===== Search for terms =====

  def searchForTerm(exprIdent: ExprIdent): TermSearchResult = {
    searchForTerm(Ident(exprIdent.head, exprIdent.tail, exprIdent.range))
  }

  def searchForTerm(name: String): TermSearchResult = {
    searchForTerm(Ident(name, List.empty, SourceRange.dummy))
  }

  def searchForTerm(ident: Ident): TermSearchResult = {
    // a.b.c
    def rec(head: String, tail: List[String]): TermSearchResult = {
      // head = a; tail = List(b, c)
      tail match {
        case nextHead :: nextTail =>
          typeSymbolTable.get(head) match {
            case Some(classSymbol: ClassSymbol) =>
              classSymbol.scope.searchForTerm(
                Ident(nextHead, nextTail, ident.range)
              ) match {
                case None =>
                  searchInParent() // (b.c) didn't get resolved correctly
                case res => res // Full a.b.c got resolved
              }
            case _ => searchInParent() // a doesn't exist in current scope or is not a class
          }
        case Nil =>
          termSymbolTable.get(head) match {
            case Some(symbol: NonOverloadableTermSymbol) => Some(symbol)
            case Some(symbols: ListBuffer[OverloadableTermSymbol]) =>
              NonEmptyList.fromList(symbols.toList)
            case None => searchInParent()
          }
      }
    }

    def searchInParent(): TermSearchResult = {
      outerScopes.iterator
        .map(_.searchForTerm(ident))
        .find(_.nonEmpty)
        .flatten
    }

    ident match {
      case Ident(head, tail, _) => rec(head, tail)
    }
  }

  def searchForOverloadableTerm(name: String): Option[NonEmptyList[OverloadableTermSymbol]] = {
    searchForTerm(name).collect {
      case res: NonEmptyList[OverloadableTermSymbol] @unchecked => res
    }
  }
  
  def searchForOverloadableTerm(name: Ident): Option[NonEmptyList[OverloadableTermSymbol]] = {
    searchForTerm(name).collect {
      case res: NonEmptyList[OverloadableTermSymbol] @unchecked => res
    }
  }

  def searchForOverloadableTerm(name: ExprIdent): Option[NonEmptyList[OverloadableTermSymbol]] = {
    searchForTerm(name).collect {
      case res: NonEmptyList[OverloadableTermSymbol] @unchecked => res
    }
  }

  def searchForNonOverloadableTerm(name: String): Option[NonOverloadableTermSymbol] = {
    searchForTerm(name).collect {
      case res: NonOverloadableTermSymbol => res
    }
  }

  def searchForNonOverloadableTerm(name: Ident): Option[NonOverloadableTermSymbol] = {
    searchForTerm(name).collect {
      case res: NonOverloadableTermSymbol => res
    }
  }

  def searchForNonOverloadableTerm(name: ExprIdent): Option[NonOverloadableTermSymbol] = {
    searchForTerm(name).collect {
      case res: NonOverloadableTermSymbol => res
    }
  }

  // ===== Define new symbols =====

  def define(symbol: Symbol): Unit = {
    symbol match {
      case symbol: TypeSymbol =>
        typeSymbolTable(symbol.name) = symbol
      case symbol: OverloadableTermSymbol =>
        termSymbolTable.get(symbol.name) match {
          case Some(buffer: ListBuffer[OverloadableTermSymbol]) => buffer += symbol
          case _ => termSymbolTable(symbol.name) = ListBuffer(symbol)
        }
      case symbol: NonOverloadableTermSymbol =>
        termSymbolTable(symbol.name) = symbol
      case _: TranslationUnit => // TranslationUnits are not stored in scope
    }
  }

  def defineAll(symbols: List[Symbol]): Unit = {
    symbols.foreach(define)
  }

  def addOuterScope(outerScope: Scope): Unit = {
    outerScopes += outerScope
  }

  type TypeSymbolTable = mutable.Map[String, TypeSymbol]
  type TermSymbolTable = mutable.Map[String, NonOverloadableTermSymbol | ListBuffer[OverloadableTermSymbol]]
  type TermSearchResult = Option[NonEmptyList[OverloadableTermSymbol] | NonOverloadableTermSymbol]

  protected val typeSymbolTable: TypeSymbolTable = mutable.Map.empty
  protected val termSymbolTable: TermSymbolTable = mutable.Map.empty

  private val outerScopes: mutable.ListBuffer[Scope] = mutable.ListBuffer.empty
}
