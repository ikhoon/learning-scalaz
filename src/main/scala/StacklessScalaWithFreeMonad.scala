/**
  * Created by ikhoon on 2016. 9. 30..
  */

// Reference : http://blog.higher-order.com/assets/trampolines.pdf
package stackless

import scala.annotation.tailrec

case class State[S, +A](runS: S => (A, S)) {
  // 기존에 함수를 적용시키고 나온 결과 값만 변형시킨다 state는 그대로 유지된다.
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = runS(s)
      (f(a), s1)
    })

  // 기존에 함수를 적용을 시키고 난 값을 이용하여 함수에 적용후 상태값을 주입하다.
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = runS(s)
      f(a) runS s1
    })
}

object State {
  def getState[S]: State[S, S] =
    State(s => (s, s))

  def setState[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def pureState[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // 이 예제는 State Monad의 적절한 사용법은 아니지만, 간단히 stackover flow를 설명하기 위해 만들었음.
  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(pureState[Int, List[(Int, A)]](List())) {
      case (acc, a) => for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs
    }.runS(0)._1.reverse

  def zipIndexWithFlatMap[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(pureState[Int, List[(Int, A)]](List())) {
      case (acc, a) =>
        acc.flatMap { (xs: List[(Int, A)]) =>
          getState.flatMap { (n: Int) =>
            setState(n + 1).map { (_: Unit) => (n, a) :: xs }
        }
      }
    }.runS(0)._1.reverse

  // 코드를 좀더 풀어보자.
  // 쉽지는 않다.
  // State는 함수다.
  // 다만 보이지 않는 State를 다룬다.
  // State는 처음 시작하는곳 runS 외에는 어디에도 노출되지 않는다.
  // 이로서 완벽한 encapsulation이 이루어 진다.
  // debugger로 실행시켜 보면 flatMap flatMap .... 으로 stack이 쌓여져 있는것을 볼수 있다.
  def zipIndexWithFlatMapState[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(pureState[Int, List[(Int, A)]](List())) {
      case (acc, a) =>
        acc.flatMap { xs =>
          State[Int, Int](s => (s, s)).flatMap { n =>
            State[Int, Unit](_ => ((), n + 1)).map { _ =>
              (n, a) :: xs
            }
          }
        }
    }.runS(0)._1.reverse
  // runS를 하기전에는 그냥 함수의 조합이 만들어져있는것이다.
  // 실행된것은 아무것도 없다.
  // 함수만 만든것이다.

}

object Problem {
  // 컴파일러에 의해서 optimize가 될수 있다.
  def foldl[A, B](as: List[A], b: B)(f: (B, A) => B): B =
    as match {
      case x :: xs => foldl(xs, f(b, x))(f)
      case Nil => b
    }

  // 요런식으로 Loop을 이용한 코드로 컴파일러에 의해 바뀔것이다.
  // stack에서 heap을 쓰는 코드로 바뀜으로 인해서 stack overflow를 피할수 있고
  // while loop을 통한 jump는 함수호춟보다 빠르게 동작한다.
  def foldlLoop[A, B](as: List[A], b: B)(f: (B, A) => B): B = {
    var z: B = b
    var az: List[A] = as

    while(true) {
      az match {
        case x :: xs =>
          z = f(z, x)
          az = xs
        case Nil => return z
      }
    }
    z
  }

  // 그러나 mutual recursion은 컴파일러에 의해서 최적화 되지 않는다.
  // 아래와 같은 코드는 최적화 될수 없다.
  def even[A](ns: List[A]): Boolean = ns match {
    case Nil => true
    case x :: xs => odd(xs)
  }

  def odd[A](ns: List[A]): Boolean = ns match {
    case Nil => false
    case x :: xs => even(xs)
  }

}
// Trampoline의 자료 구조형에 대해서 정의해보자.
sealed trait Trampoline[+A] {
  // final로 정의한 이유는 scala compiler가 tail call을 없애주기 때문이다.
  /**
  @tailrec
  final def runT_: A = this match {
    case More(k) => k().runT
    case Done(v) => v
  }
 */

  @tailrec
  final def runT: A = resume match {
    case Right(v) => v
    case Left(k) => k().runT
  }

  // Trampoline에 flatMap을 구현한다.
  // f에 runT의 결과를 넘겨준다. 그리고 그 함수를 More 감싸서 그 안에서 실행되도록 한다.
  // f(runT)가 다시 runT에 의해서 실행이 된다.
  @deprecated
  def flatMap_[B](f: A => Trampoline[B]): Trampoline[B] =
  More[B](() => f(runT))

  // 그러나 trampoline data 타입의 constructor을 Trampoline의 데이터 타입에 넣는 것은 한계가 있다
  // 상속 받은 애들이 constructor니까?

  // 그래서 flatMap을 함수에서 생성자를 호출하는걸로 바꾸어 보자.


  // 그리고 다음단계에 진행에 대한 고려를 분리해보자.
  @deprecated
  final def _resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(k) => Left(k)
    case FlatMap(a, f) => a match {
      case Done(v) => f(v)._resume
      case More(k) => Left(() => FlatMap(k(), f))
      case FlatMap(b, g) => (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A])._resume
    }
  }

  // resume의 FlatMap case에 대한 구현은 쉽지 않다.
  // * Done의 경우 계속해서 실행한다.
  // * More로 되어 있을경우 FlatMap안에서 하나의 step을 가도록 한다
  // * FlatMap이 하위 subroutine을 가지고 있을경우(FlatMap)을 또가지고 있을경우에
  // 즉 FlatMap(a, f) == FlatMap(FlatMap(b, g), f) 가 될때
  // 새로운 스택을 만들지 않고 만드는 것을 유지하는것이 이문제를 푸는데 중요한 포인트이다.
  // 트릭은 오른쪽으로 다시 관계를 형성하도록 하는것이다. 오른쪽 결합의 법칙
  // FlatMap(b, x => FlatMap(g(x), f))
  // Trampoline은 모나드이다. 그러므로 오른쪽 결합의 법칙이 성립해야한다.


  // FlatMap이 왼쪽으로 치우져져 있으면 resume은 stack overflow가 날수 있다.
  // f(v) 가 g(x)를 호출할것이고 이것은 또다른 inner call을 발생시킨다.

  // 생성자(FlatMap)이 왼쪽으로 중첩연결되어 있는것을 피하기 위해서 FlatMap의 생성자를 private으로 바꿀것이다.
  // 그리고 Trampoline에 flatMap 함수를 노출할것이다.
  // flatMap함수는 FlatMap을 항상 오른쪽 연결로 결합을 통한 생성이 이루어 지도록 다시 작성할것이다.

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this match {
      case FlatMap(b, g) => FlatMap(b, (x: Any) => g(x) flatMap f)
      case x => FlatMap(x, f)
    }

  // 그리고 resume에서도 FlatMap 생성자를 호출하는것을 flatMap 함수를 호출하는 것으로 치환할것이다.
  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(k) => Left(k)
    case FlatMap(a, f) => a match {
      case Done(v) => f(v).resume
      case More(k) => Left(() => FlatMap(k(), f))
      case FlatMap(b, g) => b.flatMap((x: Any) => g(x).flatMap(f)).resume
    }
  }


  // 마지막으로 Trampoline을 monad로 scala for comprehesion에서 사용하기 위해서 map을 구현해보자
  // flatMap을 이용해서 구현하면 된다.
  // Done이 constructor이다.

  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Done(f(a)))



}

// flatMap을 case class로 표현해보자.
case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

// 재귀적으로 계속 실행한다.
case class More[+A](k : () => Trampoline[A]) extends Trampoline[A]
// 끝내고 값을 반환한다.
case class Done[+A](result: A) extends Trampoline[A]

// 위의 mutual recursion을 Trampoline으로 다시 구현해보자.
object Trampoline {
  // 구현은 거의 같다.
  // 똑같이 odd 함수를 호출은 한다.
  // 다만 그 함수가 More에 들어있다. 바로 실행되지는 않는다.
  // odd를 실행하는 부분은 Trampoline의 runT이다.
  // runT는 tail recursive하다.
  // 즉 runT에 의해서 최적화 된다.
  // 간단한 코드의 구현이지만 역시나 아이디어가 좋다.
  // Trampoline의 개념은 scala.util.control.TailCalls 에 구현되어 있다.
  def even[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(true)
    case x :: xs => More(() => odd(xs))
  }

  // 이부분도 위와 같다.
  def odd[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(false)
    case x :: xs => More(() => even(xs))
  }
  // 그러면 모든 호출을 tail call로 만들어 보자.
  // 기존의 State를 변경시켜 보자.
  // `runS: S => (A, S)`를 `runS : S => Trampoline[(A, S)]`로 바꾸어 보자.
  case class State[S, +A](runS: S => Trampoline[(A, S)]) {

    // 현재 state의 실행결과(a, s1)중 a를 f에 적용시키고 난 state에 runS를 통해서 s1을 실행시키고 그결과를 반환한다.
    // 볼때마다 헷갈림 ㅠ.ㅠ

    // 1번째 구현
    // 이구현은 stack overflow를 발생시킨다.
    // runT가 tail call 위치에 있지 않기 때문이다.
    // Trampoline으로 감싸주어야 한다. 그렇지 않으면 최적화가 되지 않는다.
    def flatMap_[B](f: A => State[S, B]): State[S, B] =
      State(s => More(() => {
        val (a, s1) = runS(s).runT
        More(() => f(a) runS s1)
      }))

    // Trampoline monadic 을 활용해서 이문제를 풀어보자.
    // Trampoline의 Done 연산자는 pure, point의 역활는 하는 생성자이다.

    // Trampoline에 flatMap을 구현하여 State의 flatMap에서 사용하여 보자.
    // runT를 직접적으로 호출하는 대신 flatMap으로 바꾸었다.
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => More(() => runS(s).flatMap {
        case (a, s1) => More(() => f(a) runS s1)
      }))


  }

}





