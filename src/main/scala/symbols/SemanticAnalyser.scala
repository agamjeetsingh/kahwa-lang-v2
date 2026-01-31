package symbols

import ast.{ClassDecl, Decl, FunctionDecl, KahwaFile, TypeParameterDecl, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.SymbolAlreadyDeclared
import sources.SourceRange

import scala.collection.mutable.ListBuffer

object SemanticAnalyser {
  def processFile(kahwaFile: KahwaFile): TranslationUnit = {
    val res = declareFile(kahwaFile)

    ???
  }

  private def register[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                               decls: List[T],
                                               declToSymbolAndRange: T => (U, SourceRange),
                                               registerSymbol: U => Unit,
                                               duplicatesAllowed: Boolean, term: Boolean): List[Diagnostic] = {
    val ts = decls.map(decl => (declToSymbolAndRange(decl), decl))

    ts.flatMap(tuple => {
      val ((childSymbol, range), decl) = tuple
      if (!duplicatesAllowed && ((term && parentSymbol.scope.searchForTerm(childSymbol.name).nonEmpty) || parentSymbol.scope.searchForType(childSymbol.name).nonEmpty)) {
        List(SymbolAlreadyDeclared(childSymbol.name, range))
      } else {
        parentSymbol.scope.define(childSymbol)
        registerSymbol(childSymbol)
        List.empty
      }
    })
  }

  private def registerTerm[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                          decls: List[T],
                          declToSymbolAndRange: T => (U, SourceRange),
                          registerSymbol: U => Unit,
                          duplicatesAllowed: Boolean = false): List[Diagnostic] = {
    register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, true)
  }

  private def registerType[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                   decls: List[T],
                                                   declToSymbolAndRange: T => (U, SourceRange),
                                                   registerSymbol: U => Unit,
                                                   duplicatesAllowed: Boolean = false): List[Diagnostic] = {
    register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, false)
  }

  private def declareFile(kahwaFile: KahwaFile): TranslationUnit = {
    val translationUnit = TranslationUnit(kahwaFile.range.fileId.toString, List.empty)

    registerType(
      translationUnit,
      kahwaFile.classDecls,
      classDecl => (declareClass(classDecl, translationUnit.scope), SourceRange.dummy),
      translationUnit.classes += _)
    
    translationUnit
  }

  private def declareClass(classDecl: ClassDecl, outerScope: Scope): ClassSymbol = {
    val classSymbol = ClassSymbol(classDecl.name, outerScope)

    registerType(
      classSymbol,
      classDecl.typeParameters,
      (typeParameterDecl : TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, classSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
      classSymbol.genericArguments += _)

    registerType(
      classSymbol,
      classDecl.nestedClasses,
      nestedClassDecl => (declareClass(nestedClassDecl, classSymbol.scope), classDecl.range),
      classSymbol.nestedClasses += _
    )

    registerTerm(
      classSymbol,
      classDecl.methods,
      methodDecl => (declareMethod(methodDecl, classSymbol.scope), methodDecl.range),
      classSymbol.methods += _,
      duplicatesAllowed = true
    )

    registerTerm(
      classSymbol,
      classDecl.fields,
      variableDecl => (declareField(variableDecl, classSymbol.scope), variableDecl.range),
      classSymbol.fields += _
    )

    classSymbol
  }

  private def declareMethod(functionDecl: FunctionDecl, outerScope: Scope): MethodSymbol = {
    val methodSymbol = MethodSymbol(functionDecl.name, outerScope)

    methodSymbol
  }
  
  private def declareField(variableDecl: VariableDecl, outerScope: Scope): FieldSymbol = {
    ???
  }
}
