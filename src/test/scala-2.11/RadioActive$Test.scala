import RadioActive.combinations

class RadioActive$Test extends org.scalatest.FunSuite {

  val balls = (1 to 15).toList
  val c2_15 = combinations(balls, 2).toList

  test("test order") {
    println(RadioActive.powerSet((1 to 3).toList).toList)
  }

  test("C(15,2) = 105") {
    assert(c2_15.length == 105)
  }

  test("C(4,2) = 6") {
    assert(combinations(List(1, 2, 3), 2).length == 3)
  }

  test("test1") {
    assert(RadioActive.beep(c2_15, List(2, 7)).length == 27)
  }

  test("test1 1 << 3 = 8") {
    assert((1 << 3) == 8)
  }

  test("no way to check 105 combination with 2^6 tests") {
    assert(RadioActive.solvable(c2_15, 6))
  }

  test("2 radioactive among 15 is solvable with 7 tests") {
    val solutions = RadioActive.solve(combinations((1 to 15).toList, 2).toList, 7)
    println(solutions.filter(_.isDefined).head.get)
  }

  test("2 radioactive among 11 is not solvable with 6 tests") {
    assert(!RadioActive.solvable(combinations((1 to 11).toList, 2).toList, 6))
  }

  test("couple of them") {
    assert(RadioActive.solvable(combinations((1 to 6).toList, 2).toList, 4))
  }

  test("simple") {
    assert(RadioActive.solvable(combinations((1 to 1).toList, 1).toList, 0))
    assert(RadioActive.solvable(combinations((1 to 1).toList, 1).toList, 1))
    assert(RadioActive.solvable(combinations((1 to 2).toList, 1).toList, 1))
    assert(RadioActive.solvable(combinations((1 to 4).toList, 1).toList, 3))
    assert(RadioActive.solvable(combinations((1 to 4).toList, 1).toList, 2))
    assert(!RadioActive.solvable(combinations((1 to 5).toList, 1).toList, 2))
  }

  test("println") {
    println(RadioActive.solve(combinations((1 to 4).toList, 1).toList, 3).filter(_.isDefined).head.get)
    println(RadioActive.solve(combinations((1 to 100).toList, 100).toList, 0).filter(_.isDefined).head.get)
    println(RadioActive.solve(combinations((1 to 5).toList, 2).toList, 4).filter(_.isDefined).head.get)
  }

  test("feasible") {
    assert(RadioActive.feasible(List(List(1), List(2), List(3), List(4)), 2))
    assert(!RadioActive.feasible(List(List(1), List(2), List(3), List(4), List(5)), 2))
  }

  test("testPlan") {
    println(RadioActive.testPlan((1 to 15).toList.combinations(2).toList, 7))
  }

}
