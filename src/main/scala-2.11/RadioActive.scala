import scala.collection.BitSet

object RadioActive {

  def solvable(combinations: Set[BitSet], attempts: Int): Boolean =
    solve(combinations, attempts).exists(_.isDefined)

  def solve(combinations: Set[BitSet], attempts: Int): Stream[Option[Tree[BitSet]]] =
    if (combinations.size == 1)
      Stream(Some(Leaf(combinations.head)))
    else if (attempts == 0 || combinations.isEmpty)
      Stream(None)
    else
      for {
        test <- testPlan(combinations, attempts - 1).toStream
        negativeTreeOption <- solve(test._2, attempts - 1)
        positiveTreeOption <- solve(test._3, attempts - 1)
      } yield {
        for {
          negativeTree <- negativeTreeOption
          positiveTree <- positiveTreeOption
        } yield Node(test._1, negativeTree, positiveTree)
      }

  def beep(combinations: Set[BitSet], test: BitSet): (Set[BitSet], Set[BitSet]) =
    combinations.partition(s => (s & test).isEmpty)

  def testPlan(combinations: Set[BitSet], attempts: Int): Vector[(BitSet, Set[BitSet], Set[BitSet])] =
    (for {
      test <- (BitSet.empty ++ combinations.flatMap(_.toSet)).subsets()
      testResults = beep(combinations, test)
      if (feasible(testResults._1, attempts) && feasible(testResults._2, attempts))
    } yield (test, testResults._1, testResults._2)).toVector.reverse

  def feasible[A](combinations: Set[A], attempts: Int) = combinations.size <= (1 << attempts)

  def puzzle(radioActiveBalls: Int, balls: Int): Set[BitSet] = {
    (1 to balls).toList.combinations(radioActiveBalls).map(l => BitSet.empty ++ l).toSet
  }

  def main(args: Array[String]) {
    println(RadioActive.solve(puzzle(2, 15), 7).filter(_.isDefined).head.get)
  }
}
