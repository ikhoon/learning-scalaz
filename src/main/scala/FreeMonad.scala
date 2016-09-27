import CharToy.{CharBell, CharDone, CharOutput}

import scala.language.{higherKinds, reflectiveCalls}
import scalaz._
import _root_.Free.{Gosub, Return}

import Scalaz._
/**
  * Created by ikhoon on 2016. 9. 24..
  */

// learning scalaz - Free Monad
// reference : http://eed3si9n.com/learning-scalaz/Free+Monad.html

// Toy
sealed trait Toy[+A, +Next]

case class Output[A, Next](b: A, next: Next) extends Toy[A, Next]
case class Bell[Next](next: Next) extends Toy[Nothing, Next]
case class Done() extends Toy[Nothing, Nothing]

// CharToy
// hardcode the datatype to Char
sealed trait CharToy[+Next]

object CharToy {
  case class CharOutput[Next](b: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  // added helper functions output, bell and done to unify the type to CharToy
  def output[Next](b: Char, next: Next): CharToy[Next] = CharOutput(b, next)
  def bell[Next](next: Next): CharToy[Next] = CharBell(next)
  def done: CharToy[Nothing] = CharDone()
}

// WFMM(Why Free monad Matters?):
// but unfortunately this doesn't work because every time I want to add a command, it change the type.

// Let's define Fix
// fix는 unit과 비슷한 컨샙으로 사용된거 같다.
// next를 fix로 감싼다.
case class Fix[F[_]](f: F[Fix[F]])
object Fix {
  def fix(toy: CharToy[Fix[CharToy]]): Fix[CharToy] = Fix[CharToy](toy)
}

// We are also going to implement FixE, which adds exception to this.
// Since throw and catch are reserved, renamed them to thowy and catchy
sealed trait FixE[F[_], E]
object FixE {
  case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
  case class Throwy[F[_], E](e: E) extends FixE[F, E]

  def fix[E](toy: CharToy[FixE[CharToy, E]]): FixE[CharToy, E] = Fix[CharToy, E](toy)
  def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)
  // Throwy가 올때까지 계속 수행한다.
  def catchy[F[_]: Functor, E1, E2](ex: FixE[F, E1])(f: E1 => FixE[F, E2]): FixE[F, E2] =
    ex match {
      case Fix(x: F[FixE[F, E1]]) => Fix[F, E2](Functor[F].map(x) { (xx: FixE[F, E1]) => catchy(xx)(f) })
      case Throwy(e) => f(e)
    }

  // We only use this if Toy b is functor
  // Let's define functor for CharToy
  // Functor로 만드는 이유는 map을 통해서 recursive하게 수행하기 위해서 인거 같다.
  implicit val charToyInstance = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] =
      fa match {
        case CharOutput(b, next) => CharOutput(b, f(next))
        case CharBell(next) => CharBell(f(next))
        case CharDone() => CharDone()
      }
  }
}

// Free Monad Part 1
// WFMM: our FixE already exists, too, and it's called the Free Monad.

sealed abstract class Free[S[+_], +A](implicit S: Functor[S]) {
  final def map[B](f: A => B): Free[S, B] = flatMap(a => Return(f(a)))

  // flatMap의 구현이 이해가 잘되지 않는다.
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a => Gosub(a, f)
  }

}

object Free extends FreeInstances {
  // Return from the computation with the given value.
  case class Return[S[+_], +A](a: A) extends Free[S, A]
  // Suspend the computation with the given suspension.
  case class Suspend[S[+_], +A](s: S[Free[S, A]]) extends Free[S, A]
  // Call a subroutine and continue with the given function.
  case class Gosub[S[+_], +A, +B](a: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

}

trait FreeInstances {
  implicit def freeMonad[S[+_]: Functor]: Monad[({type l[x] = Free[S, x]})#l] =
    new Monad[({type l[x] = Free[S, x]})#l] {
      override def bind[A, B](fa: Free[S, A])(f: A => Free[S, B]): Free[S, B] = fa flatMap f
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa.map(f)
      override def point[A](a: => A): Free[S, A] = Return(a)
    }
}





