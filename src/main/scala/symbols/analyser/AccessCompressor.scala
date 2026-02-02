package symbols.analyser

import ast.{AstTransformer, Expr, Ident, MemberAccessExpr}

object AccessCompressor extends AstTransformer {
  override def transform(expr: Expr): Expr = {
    expr match {
      case MemberAccessExpr(base, member, range) => transform(base) match {
        case ident: Ident => Ident(ident.head, ident.tail ++ List(member), expr.range)
        case _ => super.transform(expr)
      }
      case _ => super.transform(expr)
    }
  }
}
