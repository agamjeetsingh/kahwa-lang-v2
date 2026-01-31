package symbols

enum Visibility {
  case PUBLIC
  case PRIVATE
  case PROTECTED
}

object Visibility {
  val default: Visibility = PUBLIC
}