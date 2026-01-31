package symbols

import ast.Modifier

enum Visibility {
  case PUBLIC
  case PRIVATE
  case PROTECTED
}

object Visibility {
  val default: Visibility = PUBLIC

  def fromModifier(modifier: Modifier): Visibility = {
    modifier match {
      case Modifier.PUBLIC => PUBLIC
      case Modifier.PRIVATE => PRIVATE
      case Modifier.PROTECTED => PROTECTED
      case _ => throw IllegalArgumentException(s"Visibility.fromModifier received a non-visibility modifier: ${modifier.prettyPrint}")
    }
  }
}