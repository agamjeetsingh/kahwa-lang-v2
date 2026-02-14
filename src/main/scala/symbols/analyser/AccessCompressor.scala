package symbols.analyser

import ast.{AstTransformer, Expr, ExprIdent, Ident, MemberAccessExpr}

object AccessCompressor extends AstTransformer {
  override def transform(expr: Expr): Expr = {
    expr match {
      case MemberAccessExpr(base, member, range) =>
        transform(base) match {
          case ident: ExprIdent =>
            ExprIdent(ident.head, ident.tail ++ List(member.head) ++ member.tail, expr.range)
          case _ => super.transform(expr)
        }
      case _ => super.transform(expr)
    }
  }
}
