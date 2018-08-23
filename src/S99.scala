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
  // P06
  def isPalindrome[A](list: List[A]): Boolean = list == reverse(list)
  // P07
  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (x @ y :: ys) :: ls => flatten(x) ::: flatten(ls)
    case x :: xs => x :: flatten(xs)
  }
  //P08
  def compress[A](list: List[A]): List[A] = list.foldRight(Nil: List[A]){
    (e, ls) => ls match {
      case Nil => e :: Nil
      case x @ `e` :: xs => e :: xs
      case x :: xs => e :: x :: xs
    }
  }
  // P09
  def pack[A](list: List[A]): List[List[A]] = list.foldRight(Nil: List[List[A]]){
    (e, ls) => ls match {
      case (x @ `e` :: _) :: xs => (e :: x) :: xs
      case _ => List(e) :: ls
    }
  }
  // P10
  def encode[A](list: List[A]): List[(Int, A)] = pack(list).map{
    ls => (length(ls), ls.head)
  }
  // P11
  def encodeModified[A](list: List[A]): List[Any] = encode(list).map{
    (n, e) => n match {
      case 1 => e
      case _ => (n, e)
    }
  }
}

