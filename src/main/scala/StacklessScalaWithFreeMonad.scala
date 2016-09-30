/**
  * Created by ikhoon on 2016. 9. 30..
  */

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
}



