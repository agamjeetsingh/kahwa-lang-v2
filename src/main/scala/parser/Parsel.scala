package parser

type SafePointFunction[Token] = Token => Boolean
type ParserFunction[A, Token, Error] = Parsel.Input[Token] => SafePointFunction[Token] ?=> (Option[A], Parsel.Input[Token], Iterable[Error])

class Parsel[+A, Token, Error](val parserFunc: ParserFunction[A, Token, Error]) {
  def apply(input: Parsel.Input[Token])(using spFunc: SafePointFunction[Token]): (Option[A], Parsel.Input[Token], Iterable[Error]) = parserFunc(input)

  def map[B](f: A => B)(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] = Parsel((input: Parsel.Input[Token]) =>
    val (res: Option[A], next: Parsel.Input[Token], errors: Iterable[Error]) = parserFunc(input)
    (res.map(f), next, errors))

  def ~[B](that: Parsel[B, Token, Error]): Parsel[(A, B), Token, Error] =
    Parsel((input: Parsel.Input[Token]) =>
      val (r1, i1, e1) = parserFunc(input)
      r1 match
        case None =>
          (None, input, e1)
        case Some(a) =>
          val (r2, i2, e2) = that.parserFunc(i1)
          (r2.map(b => (a, b)), i2, e1 ++ e2)
    )

  def <~[B](that: Parsel[B, Token, Error])(using spFunc: SafePointFunction[Token]): Parsel[A, Token, Error] =
    (this ~ that).map(_._1)

  def ~>[B](that: Parsel[B, Token, Error])(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] =
    (this ~ that).map(_._2)
}

object Parsel {
  case class Input[T](source: Vector[T], index: Int) { 
    def current: Option[T] = source.lift(index) 
    def advance: Input[T] = copy(index = index + 1)
    def last: Option[T] = {
      if (source.isEmpty) {
        None
      } else {
        Some(source(source.size - 1))
      }
    }
  }

  type EnforceStructure[T <: Tuple, K, Token, Error] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case _ *: t => (K, Parsel[Any, Token, Error]) *: EnforceStructure[t, K, Token, Error]

  type UnionOfParsers[T <: Tuple, Token, Error] = T match
    case EmptyTuple => Nothing
    case (_, Parsel[r, Token, Error]) *: t => r | UnionOfParsers[t, Token, Error]

  def or[K, T <: Tuple, Token, Error](discriminator: Parsel.Input[Token] => Option[K], args: T,
                        keyFoundButNotInArgsList: K => Iterable[Error] = (_: K) => Iterable.empty)(using
                                                                        // Ensure input matches ((K, Parser), (K, Parser)...)
                                                                        ev: T <:< EnforceStructure[T, K, Token, Error]
                       ): Parsel[UnionOfParsers[T, Token, Error], Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      val keyOpt = discriminator(input)

      keyOpt match {
        case Some(key) =>
          // We convert the tuple to a List to find the matching key.
          // We cast to the upper bound (K, Parser[Any]) to make it workable at runtime.
          val entries = args.toList.asInstanceOf[List[(K, Parsel[Any, Token, Error])]]

          entries.find(_._1 == key) match {
            case Some((_, parsel)) =>
              // Run the selected parser.
              // The compiler knows the result is part of UnionOfParsers[T]
              // because of the logic, but at runtime, we cast strictly for safety.
              parsel(input).asInstanceOf[(Option[UnionOfParsers[T, Token, Error]], Parsel.Input[Token], Iterable[Error])]
            case None =>
              // Key found by discriminator, but not in the args list
              (None, input, keyFoundButNotInArgsList(key))
          }
        case None =>
          // Discriminator couldn't decide
          (None, input, Iterable.empty)
      }
    })
  }
}