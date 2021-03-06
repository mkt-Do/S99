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
  // P12
  def decode[A](list: List[(Int, A)]): List[A] = {
    def create[A](n: Int, e: A): List[A] = n match {
      case 1 => e :: Nil
      case n if n <= 0 => throw new IllegalArgumentException("not allowed under 0")
      case n => e :: create(n - 1, e)
    }
    list.foldRight(Nil: List[A]){ (n, ls) => create(n._1, n._2) ::: ls }
  }
  // P13
  def encodeDirect[A](list: List[A]): List[(Int, A)] = list.foldRight(Nil: List[(Int, A)]){
    (e, ls) => ls match {
      case (n, x @ `e`) :: xs => (n + 1, e) :: xs
      case _ => (1, e) :: ls
    }
  }
  // P14
  def duplicate[A](list: List[A]): List[A] = list.foldRight(Nil: List[A]){ (e, ls) => e :: e :: ls }
  // P15
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    def createDuplicateElements[A](n: Int, e: A): List[A] = n match {
      case 0 => Nil
      case n if n < 0 => throw new IllegalArgumentException("Not allowed under 0")
      case n => e :: createDuplicateElements(n - 1, e)
    }
    list.foldRight(Nil: List[A]){ (e, ls) => createDuplicateElements(n, e) => ls }
  }
  // P16
  def drop[A](n: Int, list: List[A]): List[A] = {
    def dropMth[A](m: Int, ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case h :: t if m == 1 => dropNth(n, t)
      case h :: t if m < 1 => throw new IllegalArgumentException("Not allowed under 0")
      case h :: t => h :: dropMth(m - 1, t)
    }
  }
  // P17
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = list match {
    case Nil => (Nil, Nil)
    case h :: t if n == 1 => (h :: Nil, t)
    case h :: t if n < 1 => throw new IllegalArgumentException("Not allowed under 0")
    case h :: t => split(n - 1, t) match {
      case (xs, ys) => (h :: xs, ys)
    }
  }
  // P18
  def slice[A](m: Int, n: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: t if m == 0 && n == 0 => Nil
    case h :: t if m == 0 => h :: slice(m, n - 1, t)
    case h :: t if n == 0 => throw new IllegalArgumentException("First element is under second element")
    case h :: t => slice(m - 1, n - 1, t)
  }
  // P19
  def rotate[A](n: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: t if n < 0 => rotate(list.length + n, h :: t)
    case h :: t if n == 0 => h :: t
    case h :: t => rotate(n - 1, t :+ h)
  }
  // P20
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = list match {
    case Nil => throw new IllegalArgumentException("Nil")
    case h :: t if n == 0 => (t, h)
    case h :: t if n < 0 => throw new IllegalArgumentException("Not allowed under 0")
    case h :; t => removeAt(n - 1, t) match {
      case (ls, x) => (h :: ls, x)
    }
  }
  // P21
  def insertAt[A](e: A, n: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: t if n == 0 => e :: h :: t
    case h :: t if n < 0 => throw new IllegalArgumentException("Not allowed under 0")
    case h :: t => h :: insertAt(e, n - 1, t)
  }
  // P22
  def range(m: Int, n: Int): List[Int] = if (m > n) throw new IllegalArgumentException(s"First element $m should be under second element $n")
    else if (m == n) n :: Nil
    else m :: range(m + 1, n)
  // P23
  def randomSelect[A](n: Int, list: List[A]): List[A] = removeAt((Math.random * list.length).toInt, list) match {
    case (ls, e) if n == 1 => e :: Nil
    case (ls, e) if n <= 0 => throw new IllegalArgumentException("First element should be smaller than list size")
    case (ls, e) => e :: randomSelect(n - 1, ls)
  }
  // P24
  def lotto(n: Int, m: Int): List[Int] = {
    if (m <= 0) throw new IllegalArgumentException("Second element should be over 0")
    else if (n < 0 || m < n) throw new IllegalArgumentException("First element should be under Second element and over 0")
    else randomSelect(n, (1 to m).toList)
  }
  // P25
  def randomPermute[A](list: List[A]): List[A] = randomSelect(list.length, list)
  // P26
  def combinations[A](n: Int, list: List[A]): List[List[A]] = {
    def addHead[A](h: A, ls: List[List[A]]): List[List[A]] = ls map { h :: _ }
    list match {
      case _ if n < 1 => Nil
      case Nil => Nil
      case _ if n == 1 => list map { List(_) }
      case h :: t => addHead(h, combinations(n - 1, t)) ::: combinations(n, t)
    }
  }
  // P27a
  def group3[A](list: List[A]): List[List[List[A]]] = for {
    a <- combinations(2, list)
    b <- combinations(3, list.filter(e => !a.contains(e)))
    c <- List(list.filter(e => !(a.contains(e) || b.contains(e))))
  } yield List(a, b, c)
  // P27b
  def group[A](nums: List[Int], list: List[A]): List[List[List[A]]] = {
    if (nums.foldRight(0)((v, acc) => v + acc) > list.length) throw new IllegalArgumentException("Sum of first list should be smaller than second list length")
    nums match {
      case Nil => List(Nil)
      case x :: xs => combinations(x, list).flatMap{ ls => group(xs, list.filter(!ls.contains(_))).map{ ls :: _ } }
    }
  }
  // P28a
  def lsort[A](list: List[List[A]]): List[List[A]] = list.sortWith(_.length < _.length)
}

