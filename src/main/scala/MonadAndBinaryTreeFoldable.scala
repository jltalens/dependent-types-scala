object MonadAndBinaryTreeFoldable {

  //First part

  trait Monad[M[_]] {
    def bind[A, B](ma: M[A], k: A => M[B]): M[B]
    def pure[A](a: A): M[A]
  }

  implicit object listMonad extends Monad[List] {
    override def bind[A, B](ma: List[A], k: A => List[B]): List[B] = ma.flatMap(k)
    override def pure[A](a: A): List[A] = List(a)
  }

  implicit class ListWithBind[A, B](l: List[A]) {
    def >>=(f: A => List[B]): List[B] = implicitly[Monad[List]].bind(l, f)
  }

  implicit class WithPure[A](a: A) {
    def pure(): List[A] = implicitly[Monad[List]].pure(a)
  }

  val as: List[String] = List("a", "b", "c")
  val k: String => List[String] = x => List(x + "0" , x + "1")

  println(implicitly[Monad[List]].bind(as, k))

  println(as >>= k)

  println(implicitly[Monad[List]].pure("a"))

  println("a".pure)

  // Second part

  trait Foldable[T[_]] {
    def foldr[A, B](ta: T[A])(f: A => B => B)(seed: B): B
  }

  sealed trait BTree[+A]
  case object Empty extends BTree[Nothing]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], a: A, right: BTree[A]) extends BTree[A]

  implicit object bTreeFoldable extends Foldable[BTree] {
    override def foldr[A, B](ta: BTree[A])(f: (A) => (B) => B)(seed: B): B = ta match {
      case Empty => seed
      case Leaf(x) => f(x)(seed)
      case Fork(l, a, r) => foldr(r)(f)(f(a)(foldr(l)(f)(seed)))
    }
  }

  implicit class FoldableBTree[A, B](t: BTree[A]) {
    def foldr(f: (A) => (B) => B)(seed: B): B = implicitly[Foldable[BTree]].foldr(t)(f)(seed)
  }

  val tree : BTree[Int] = Fork(Leaf(1), 2, Leaf(3))

  println(tree.foldr((x: Int) => (y: Int) => x + y)(0))

  println(implicitly[Foldable[BTree]].foldr(tree)((x: Int) => (y: Int) => x + y)(0)
  )
}
