object Solution extends App {
  def f[A](xs: List[A]) = xs.foldLeft(0)((i,_) => i + 1)
  println(f(io.Source.stdin.getLines.toList.map(_.trim).map(_.toInt)))
}
