package parser

import ast.{AstTransformer, AtomType, Expr, FunctionDecl, FunctionType, Ident, TupleType, TypeRef}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parser.Parsel.Input
import sources.SourceRange

class ParseTypeRefTest extends AnyFlatSpec with Matchers {
  private val atomTests = List(
    AtomType(Ident("A")), // A
    AtomType(Ident("A"), List(AtomType(Ident("B")))), // A[B]
    AtomType(Ident("A"), List(AtomType(Ident("B")), AtomType(Ident("C")))), // A[B[C]]
    AtomType(Ident("A"), List(AtomType(Ident("A"), List(AtomType(Ident("B")))))),
  )

  private val tupleTests = List(
    TupleType(List.empty),
    TupleType(List(AtomType(Ident("A")), AtomType(Ident("B")))),
  )

  private val functionTests = List(
    FunctionType(List.empty, AtomType(Ident("A"))),
    FunctionType(List(AtomType(Ident("A")), AtomType(Ident("B"))), AtomType(Ident("C")))
  )

  def nullifyRanges(typeRef: TypeRef): TypeRef = {
    object Equaliser extends AstTransformer {
      override def transform(typeRef: TypeRef): TypeRef = {
        typeRef match {
          case AtomType(name, args, range) =>
            AtomType(transform(name).asInstanceOf[Ident], args.map(transform), SourceRange.dummy)
          case TupleType(elems, range) => TupleType(elems.map(transform), SourceRange.dummy)
          case FunctionType(paramList, returnType, range) =>
            FunctionType(paramList.map(transform), transform(returnType), SourceRange.dummy)
        }
      }

      override def transform(expr: Expr): Expr = expr match {
        case ident: Ident => ident.copy(range = SourceRange.dummy)
        case _ => expr
      }
    }

    Equaliser.transform(typeRef)
  }

  given SafePointFunction[Token] = tok => false
  for (example <- atomTests) {
    "Parser" should s"parse $example correctly" in {
      println(example.prettyPrint)
      val actual = Parser.parseTypeRef(Tokeniser.tokenise(example.prettyPrint, 0)._1)._1
      actual.map(nullifyRanges) shouldBe Some(example)
    }
  }
}
