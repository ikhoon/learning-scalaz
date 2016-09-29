import CharToy.{CharBell, CharDone, CharOutput}

import scala.language.{higherKinds, reflectiveCalls}
import Free.{Gosub, Return, Suspend}

import scalaz.{Functor, Monad}
import scalaz._
import Scalaz._
import scala.annotation.tailrec
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
  // 함수를 받으면 새로운 함수를 만든다. 새로 생긴 Gosub에는 x를 인자로 받으면 이를 g에 적용한다.
  // flatMap을 하면 새로운 f가 마지막 꼬리로 드러간다!!

  // 위의 catchy와 같은 모양이다 Fix에서 xx는 g(x)를 뜻한다
  // 역시나 쉽지 않다.
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a => Gosub(a, f)
  }

  // Scalaz librar에서 복붙
  /** Evaluates a single layer of the free monad **/
  @tailrec final def resume(): (S[Free[S,A]] \/ A) =
  this match {
    case Return(a) => \/-(a)
//    case Suspend(t) => -\/(S.map(t)(Return(_)))
    case b @ Gosub(_, _) => b.a match {
      case Return(a) => b.f(a).resume
      case Suspend(t) => -\/(S.map(t)(b.f))
      case c @ Gosub(_, _) => c.a.flatMap(z => c.f(z).flatMap(b.f)).resume
    }
  }

}

object Free extends FreeInstances {
  // Return from the computation with the given value.
  case class Return[S[+_]: Functor, +A](a: A) extends Free[S, A]
  // Suspend the computation with the given suspension.
  case class Suspend[S[+_]: Functor, +A](s: S[Free[S, A]]) extends Free[S, A]
  // Call a subroutine and continue with the given function.
  case class Gosub[S[+_]: Functor, A, +B](a: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

}

trait FreeInstances {
  implicit def freeMonad[S[+_]: Functor]: Monad[({type l[x] = Free[S, x]})#l] =
    new Monad[({type l[x] = Free[S, x]})#l] {
      override def bind[A, B](fa: Free[S, A])(f: A => Free[S, B]): Free[S, B] = fa flatMap f
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa.map(f)
      override def point[A](a: => A): Free[S, A] = Return(a)
    }
}

// Let's re-implement CharToy commands based on Free.

object FreeCharToy {
  sealed trait CharToy[+Next]
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] =
      fa match {
        case CharOutput(a, n) => CharOutput(a, f(n))
        case CharBell(n) => CharBell(f(n))
        case cd @ CharDone() => cd
      }
  }

  def output[Next](a: Char): Free[CharToy, Unit] =
    Free.Suspend[CharToy, Unit](CharOutput(a, Free.Return[CharToy, Unit]()))

  def bell[Next]: Free[CharToy, Unit] =
    Free.Suspend[CharToy, Unit](CharBell(Free.Return[CharToy, Unit]()))

  def done: Free[CharToy, Unit] =
    Free.Suspend[CharToy, Unit](CharDone())
}

// Let's add liftF refactoring, We need return equivalent, which we'll call pointed.

object LiftFCharToy {
  sealed trait CharToy[+Next]
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] =
      fa match {
        case CharOutput(a, n) => CharOutput(a, f(n))
        case CharBell(n) => CharBell(f(n))
        case cd @ CharDone() => cd
      }
  }

  // Functor를 가지고 Free를 만든다.
  // Suspend(map(cmd)(Return(_)))
  def liftF[F[+_]: Functor, R](command: F[R]): Free[F, R] =
    Free.Suspend[F, R](Functor[F].map(command){Free.Return[F, R]})

  def output(a: Char): Free[CharToy, Unit] = liftF[CharToy, Unit](CharOutput(a, ()))

  def bell: Free[CharToy, Unit] = liftF[CharToy, Unit](CharBell(()))

  def done: Free[CharToy, Unit] = liftF[CharToy, Unit](CharDone())

  def pointed[A](a: A): Free[CharToy, Unit] = Free.Return[CharToy, Unit](a)

  def showProgram[R: Show](p: Free[CharToy, R]): String =
    p.resume.fold({
      case CharOutput(a, next) =>
        "output " + Show[Char].show(a) + "\n" + showProgram(next)
      case CharBell(next) =>
        "bell \n" + showProgram(next)
      case CharDone() =>
        "done\n"
    }, { r: R => "return " + Show[R].show(r) + "\n" })

}



