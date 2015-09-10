import scala.collection.BitSet

class RadioActive$Test extends org.scalatest.FunSuite {

  def puzzle(radioActiveBalls: Int, balls: Int): Set[BitSet] = {
    (1 to balls).toList.combinations(radioActiveBalls).map(l => BitSet.empty ++ l).toSet
  }

  test("C(15,2) = 105") {
    assert(puzzle(2, 15).size == 105)
  }

  test("C(4,2) = 6") {
    assert(RadioActive.combinations(BitSet(1, 2, 3), 2).length == 3)
  }

  test("test1 1 << 3 = 8") {
    assert((1 << 3) == 8)
  }

  test("no way to check 105 combination with 2^6 tests") {
    assert(RadioActive.solvable(puzzle(2, 15), 6))
  }

  test("2 radioactive among 15 is solvable with 7 tests") {
    val solutions = RadioActive.solve(puzzle(2, 15), 7)
    println(solutions.filter(_.isDefined).head.get)
  }

  test("2 radioactive among 11 is not solvable with 6 tests") {
    assert(!RadioActive.solvable(puzzle(2, 11), 6))
  }

  test("couple of them") {
    assert(RadioActive.solvable(puzzle(2, 6), 4))
  }

  test("simple") {
    assert(!RadioActive.solvable(Set(), 100))
    assert(RadioActive.solvable(puzzle(1, 1), 0))
    assert(RadioActive.solvable(puzzle(1, 1), 1))
    assert(RadioActive.solvable(puzzle(1, 2), 1))
    assert(RadioActive.solvable(puzzle(1, 4), 3))
    assert(RadioActive.solvable(puzzle(1, 4), 2))
    assert(!RadioActive.solvable(puzzle(1, 5), 2))
  }

  test("println") {
    println(RadioActive.solve(puzzle(1, 3), 3).filter(_.isDefined).head.get)
    println(RadioActive.solve(puzzle(1, 100), 0).filter(_.isDefined).head.get)
    println(RadioActive.solve(puzzle(1, 5), 4).filter(_.isDefined).head.get)
  }

  test("feasible") {
    assert(RadioActive.feasible(Set(List(1), List(2), List(3), List(4)), 2))
    assert(!RadioActive.feasible(Set(List(1), List(2), List(3), List(4), List(5)), 2))
  }

  test("beep") {
    assert(RadioActive.beep(puzzle(2, 15), BitSet(2, 7))._1.size == 105 - 27) //negative
    assert(RadioActive.beep(puzzle(2, 15), BitSet(2, 7))._2.size == 27) // positive

    assert(RadioActive.beep(Set(BitSet(1), BitSet(2), BitSet(3)), BitSet())._1.size == 3)
    assert(RadioActive.beep(Set(BitSet(1), BitSet(2), BitSet(3)), BitSet())._2.size == 0)
  }

  test("testPlan") {
    assert(RadioActive.testPlan(puzzle(2, 15), 6).size == 4368)
    assert(RadioActive.testPlan(puzzle(2, 15), 6).toList.head._1.size == 4)
  }

}
