import org.scalatest.{FunSuite, Matchers, WordSpec}

/**
  * Created by ikhoon on 2016. 9. 24..
  */
class FreeMonadSpec extends WordSpec with Matchers {

  "free monad" should {
    "toy" in {
      println(Output("b", Done()))
      println(Bell(Output("b", Done())))
    }

    "chartoy" in {
      import CharToy._
      println(output('A', done))
      println(bell(output('A', done)))
    }

    "fix" in {
      import CharToy._
      import Fix._
      println(fix(output('A', fix(done))))
      println(fix(bell(fix(output('A', fix(done))))))
    }

    "fixe" in {
      import CharToy._
      import FixE._
      case class IncompleteException()
      def subroutine: FixE[CharToy, IncompleteException] =
        fix[IncompleteException](
          output('A',
            throwy[CharToy, IncompleteException](IncompleteException())))

      println(subroutine)
      def program = catchy[CharToy, IncompleteException, Nothing](subroutine) { _ =>
        fix[Nothing](bell(fix[Nothing](done)))
      }
      println(program)

    }

    "free" in {
      import LiftFCharToy._
      val subroutine = output('A')
      val program = for {
        _ <- subroutine
        _ <- bell
        _ <- done
      } yield ()


    }
  }


}
