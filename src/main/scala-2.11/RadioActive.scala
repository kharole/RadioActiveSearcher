import scala.collection.BitSet

object RadioActive {

  object |<| {
    def unapply(s: BitSet): Option[(Int, BitSet)] =
      if (s.isEmpty) None
      else Some((s.head, s.tail))
  }

  def combinations(set: BitSet, k: Int): Stream[BitSet] =
    if (k == set.size)
      Stream(set)
    else
      set match {
        case head |<| tail => combinations(tail, k - 1).map(_ + head) ++ combinations(tail, k)
        case _ => Stream()
      }

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
}
