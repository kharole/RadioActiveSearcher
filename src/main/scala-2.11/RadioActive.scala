object RadioActive {

  def combinations(l: List[Int], k: Int): Stream[List[Int]] =
    if (k == l.length)
      Stream(l)
    else
      l match {
        case x :: xs => combinations(xs, k - 1).map(c => x :: c) ++ combinations(xs, k)
        case Nil => Stream()
      }

  def combinationsNumber(n: Int, k: Int): Int = combinations((1 to n).toList, k).length

  def solvable(combinations: List[List[Int]], attempts: Int): Boolean =
    solve(combinations, attempts).exists(_.isDefined)

  def solve(combinations: List[List[Int]], attempts: Int): Stream[Option[Tree[List[Int]]]] = combinations match {
    case head :: Nil => Stream(Some(Leaf(head)))
    case head :: tail => if (attempts == 0)
      Stream(None)
    else {
      for {
        test <- testPlan(combinations, attempts).toStream
        negativeTreeOption <- solve(test._2, attempts - 1)
        positiveTreeOption <- solve(test._3, attempts - 1)
      } yield {
        for {
          negativeTree <- negativeTreeOption
          positiveTree <- positiveTreeOption
        } yield Node(test._1, negativeTree, positiveTree)
      }
    }
  }

  def beep(possibleCombinations: List[List[Int]], test: List[Int]): List[List[Int]] =
    (for {
      t <- test
      beepCombinations <- possibleCombinations.filter(c => c.contains(t))
    } yield beepCombinations).distinct

  def powerSet(balls: List[Int]): Iterable[List[Int]] = for {
    len <- 1 to balls.length
    test <- balls.combinations(len)
  } yield test

  def testPlan(combinations: List[List[Int]], attempts: Int): List[(List[Int], List[List[Int]], List[List[Int]])] =
    for {
      test <- powerSet(combinations.flatten.distinct).toList
      positive = beep(combinations, test)
      negative = combinations diff positive
      if (feasible(negative, attempts) && feasible(positive, attempts))
    } yield (test, negative, positive)

  def feasible(combinations: List[List[Int]], attempts: Int) = combinations.length <= (1 << attempts)
}
