object S99 {
  // P01
  def last [A](list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException("Nil")
    case x :: Nil => x
    case x :: xs => last(xs)
  }
}

