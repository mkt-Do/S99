object S99 {
  // P01
  def last [A](list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException("Nil")
    case x :: Nil => x
    case x :: xs => last(xs)
  }
  // P02
  def penultimate[A](list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException("Nil")
    case x :: Nil => throw new IllegalArgumentException("Length 1")
    case x :: y :: Nil => x
    case x :: y :: ys => penultimate(y :: ys)
  }
}

