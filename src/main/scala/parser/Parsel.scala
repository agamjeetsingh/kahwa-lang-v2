package parser

type SafePointFunction[Token] = Token => Boolean
type ParserFunction[A, Token, Error] = Parsel.Input[Token] => SafePointFunction[Token] ?=> (Option[A], Parsel.Input[Token], Iterable[Error])

class Parsel[+A, Token, Error](val parserFunc: ParserFunction[A, Token, Error]) {
  def apply(input: Parsel.Input[Token])(using spFunc: SafePointFunction[Token]): (Option[A], Parsel.Input[Token], Iterable[Error]) = parserFunc(input)

  def map[B](f: A => B)(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] = Parsel((input: Parsel.Input[Token]) =>
    val (res: Option[A], next: Parsel.Input[Token], errors: Iterable[Error]) = parserFunc(input)
    (res.map(f), next, errors))

  def map[B](f: (A, Parsel.Input[Token]) => B)(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] = Parsel((input: Parsel.Input[Token]) =>
    val (res: Option[A], next: Parsel.Input[Token], errors: Iterable[Error]) = parserFunc(input)
    (res match {
      case Some(value: A) => Some(f(value, input)) // TODO - Not 100% on whether it should be input or next
      case None => None
    }, next, errors)
  )

  def flatMap[B](f: A => Option[B])(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] =
    Parsel((input: Parsel.Input[Token]) => {
      val (res, next, errors) = parserFunc(input)
      res match {
        case Some(a) => (f(a), next, errors)
        case None => (None, next, errors)
      }
    })

  def flatMap[B](f: (A, Parsel.Input[Token]) => Option[B])(using spFunc: SafePointFunction[Token]): Parsel[B, Token, Error] =
    Parsel((input: Parsel.Input[Token]) => {
      val (res, next, errors) = parserFunc(input)
      res match {
        case Some(a) => (f(a, input), next, errors) // TODO - Not 100% on whether it should be input or next
        case None => (None, next, errors)
      }
    })

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

  def or[K, Token, T <: Tuple, Error](discriminator: Parsel.Input[Token] => Option[K], args: T,
                        keyFoundButNotInArgsList: K => Iterable[Error])(using
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

  def or[A, Token, Error](parsers: Parsel[A, Token, Error]*): Parsel[A, Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      @scala.annotation.tailrec
      def tryParsers(remaining: Seq[Parsel[A, Token, Error]], lastErrors: Iterable[Error]): (Option[A], Parsel.Input[Token], Iterable[Error]) = {
        remaining.headOption match {
          case None =>
            // All parsers failed
            (None, input, lastErrors)
          case Some(parser) =>
            val (res, next, errs) = parser.parserFunc(input)
            res match {
              case Some(a) =>
                // Success - return result
                (Some(a), next, errs)
              case None =>
                // Failed - try next parser
                tryParsers(remaining.tail, errs)
            }
        }
      }

      tryParsers(parsers, Iterable.empty)
    })
  }

  def list[A, Token, Error](parsel: Parsel[A, Token, Error]): Parsel[List[A], Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      @scala.annotation.tailrec
      def loop(currentInput: Parsel.Input[Token],
               lastGoodInput: Parsel.Input[Token],
               acc: List[A],
               accErrors: Iterable[Error]): (Option[List[A]], Parsel.Input[Token], Iterable[Error]) = {
        val (res, nextInput, errs) = parsel.parserFunc(currentInput)
        res match {
          case Some(a) =>
            // Successful parse - accumulate result and errors, continue from next input
            loop(nextInput, nextInput, a :: acc, accErrors ++ errs)
          case None =>
            // Failed parse - backtrack to last good input, discard the failure errors
            (Some(acc.reverse), lastGoodInput, accErrors)
        }
      }

      loop(input, input, List.empty, Iterable.empty)
    })
  }

  def optional[A, Token, Error](parsel: Parsel[A, Token, Error]): Parsel[Option[A], Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      val (res, next, errs) = parsel.parserFunc(input)
      res match {
        case Some(a) => (Some(Some(a)), next, errs)
        case None => (Some(None), input, Iterable.empty)
      }
    })
  }

  def sepBy[A, Token, Error, B](element: Parsel[A, Token, Error], separator: Parsel[B, Token, Error]): Parsel[List[A], Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      val (firstRes, firstNext, firstErrs) = element(input)

      firstRes match {
        case None =>
          // No first element - return empty list, backtrack, no errors
          (Some(List.empty), input, Iterable.empty)
        case Some(first) =>
          // Got first element, now loop for separator + element pairs
          @scala.annotation.tailrec
          def loop(currentInput: Parsel.Input[Token],
                   acc: List[A],
                   accErrors: Iterable[Error]): (Option[List[A]], Parsel.Input[Token], Iterable[Error]) = {
            val (sepRes, sepNext, sepErrs) = separator.parserFunc(currentInput)
            sepRes match {
              case None =>
                // No separator - we're done
                (Some(acc.reverse), currentInput, accErrors)
              case Some(_) =>
                // Got separator, now MUST parse element
                val (elemRes, elemNext, elemErrs) = element.parserFunc(sepNext)
                elemRes match {
                  case Some(a) =>
                    // Got element, continue
                    loop(elemNext, a :: acc, accErrors ++ sepErrs ++ elemErrs)
                  case None =>
                    // Failed to parse element after separator - keep errors and stop
                    (Some(acc.reverse), elemNext, accErrors ++ sepErrs ++ elemErrs)
                }
            }
          }

          loop(firstNext, List(first), firstErrs)
      }
    })
  }

  def delay[A, Token, Error](parser: => Parsel[A, Token, Error]): Parsel[A, Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => parser.parserFunc(input))
  }

  sealed trait Fixity
  object Prefix extends Fixity
  object Postfix extends Fixity
  object InfixL extends Fixity
  object InfixR extends Fixity
  object InfixN extends Fixity

  sealed trait OpLevel[Expr, Token, Error] {
    def fixity: Fixity
    def ops: Seq[Parsel[Any, Token, Error]]
  }

  class Ops[Expr, Token, Error](val fixity: Fixity, val ops: Parsel[Any, Token, Error]*) extends OpLevel[Expr, Token, Error]

  object Ops {
    def apply[Expr, Token, Error](fixity: Fixity)(ops: Parsel[Any, Token, Error]*): Ops[Expr, Token, Error] =
      new Ops(fixity, ops*)
  }

  def precedence[Expr, Token, Error](
    atoms: Parsel[Expr, Token, Error]*
  )(
    levels: OpLevel[Expr, Token, Error]*
  ): Parsel[Expr, Token, Error] = {
    Parsel((input: Parsel.Input[Token]) => {
      val atomParser = or(atoms*)

      def parsePrefix(input: Parsel.Input[Token], prefixOps: List[Parsel[Any, Token, Error]]): (Option[Expr], Parsel.Input[Token], Iterable[Error]) = {
        prefixOps match {
          case Nil =>
            atomParser.parserFunc(input)
          case ops =>
            val prefixParser = or(ops.map(_.asInstanceOf[Parsel[Expr => Expr, Token, Error]])*)
            val (opRes, opNext, opErrs) = prefixParser.parserFunc(input)

            opRes match {
              case Some(f) =>
                val (exprRes, exprNext, exprErrs) = parseExprWithLevel(opNext, 0)
                (exprRes.map(f), exprNext, opErrs ++ exprErrs)
              case None =>
                atomParser.parserFunc(input)
            }
        }
      }

      def parseExprWithLevel(input: Parsel.Input[Token], minLevel: Int): (Option[Expr], Parsel.Input[Token], Iterable[Error]) = {
        val prefixOps = levels.zipWithIndex.collect {
          case (level, index) if level.fixity == Prefix && index >= minLevel => level.ops.toList
        }.flatten.toList

        val (lhsOpt, lhsNext, lhsErrs) = parsePrefix(input, prefixOps)

        lhsOpt match {
          case None => (None, lhsNext, lhsErrs)
          case Some(lhs) => parseOperators(lhs, lhsNext, lhsErrs, minLevel)
        }
      }

      def parseOperators(lhs: Expr, input: Parsel.Input[Token], accErrs: Iterable[Error], minLevel: Int): (Option[Expr], Parsel.Input[Token], Iterable[Error]) = {
        var currentLhs = lhs
        var currentInput = input
        var currentErrs = accErrs
        var continue = true

        while (continue) {
          var matched = false

          for ((level, levelIndex) <- levels.zipWithIndex if !matched && levelIndex >= minLevel) {
            level.fixity match {
              case Postfix =>
                val postfixParser = or(level.ops.map(_.asInstanceOf[Parsel[Expr => Expr, Token, Error]])*)
                val (opRes, opNext, opErrs) = postfixParser.parserFunc(currentInput)

                opRes match {
                  case Some(f) =>
                    currentLhs = f(currentLhs)
                    currentInput = opNext
                    currentErrs = currentErrs ++ opErrs
                    matched = true
                  case None =>
                }

              case fixity @ (InfixL | InfixR | InfixN) =>
                val infixParser = or(level.ops.map(_.asInstanceOf[Parsel[(Expr, Expr) => Expr, Token, Error]])*)
                val (opRes, opNext, opErrs) = infixParser.parserFunc(currentInput)

                opRes match {
                  case Some(f) =>
                    val nextMinLevel = fixity match {
                      case InfixL => levelIndex + 1
                      case InfixR => levelIndex
                      case InfixN => levelIndex + 1
                    }

                    val (rhsOpt, rhsNext, rhsErrs) = parseExprWithLevel(opNext, nextMinLevel)

                    rhsOpt match {
                      case Some(rhs) =>
                        currentLhs = f(currentLhs, rhs)
                        currentInput = rhsNext
                        currentErrs = currentErrs ++ opErrs ++ rhsErrs
                        matched = true
                      case None =>
                        currentErrs = currentErrs ++ opErrs ++ rhsErrs
                    }
                  case None =>
                }

              case Prefix =>
            }
          }

          if (!matched) {
            continue = false
          }
        }

        (Some(currentLhs), currentInput, currentErrs)
      }

      parseExprWithLevel(input, 0)
    })
  }

  def sync[Token](input: Parsel.Input[Token])(using spFunc: SafePointFunction[Token]): Parsel.Input[Token] = {
    var i = input
    while (i.current match {
      case Some(token) => !spFunc(token)
      case None => false
    }) {
      i = i.advance
    }
    i
  }
  
  def commit[A, Token, Error](parser: Parsel[A, Token, Error]): Parsel[A, Token, Error] = Parsel((input: Parsel.Input[Token]) => {
    val (res, next, errs) = parser.parserFunc(input)
    res match {
      case Some(_) => (res, next, errs)
      case None => (res, sync(next), errs)
    }
  })
}