import org.scalatest.{FunSuite, Matchers, WordSpec}

/**
  * Created by ikhoon on 2016. 10. 1..
  */
class StacklessScalaWithFreeMonadSpec extends WordSpec with Matchers {

  "trampoline" should {

    import stackless._
    import State._
    val l = List(1, 2, 3, 4, 5, 6)

    "zipIndex" in {
      zipIndex(l) shouldBe List((0,1), (1,2), (2,3), (3,4), (4,5), (5,6))
    }

    "zipIndexWithFlatMap" in {
      zipIndexWithFlatMap(l) shouldBe List((0,1), (1,2), (2,3), (3,4), (4,5), (5,6))
    }

    val l2 = List(10, 20, 30, 40, 50, 60)
    "zipIndexWithFlatMapState" in {
      zipIndexWithFlatMapState(l2) shouldBe List((0,10), (1,20), (2,30), (3,40), (4,50), (5,60))
    }
  }
}
