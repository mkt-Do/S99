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
  // P03
  def nth[A](n: Int, list: List[A]): A = list match {
    case Nil => throw new IllegalArgumentException("Nil")
    case x :: xs if n == 0 => x
    case x :: xs => nth(n - 1, xs)
  }
  // P04
  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case x :: xs => length(xs) + 1
  }
  // P05
  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case x :: xs => reverse(xs) :+ x
  }
}

