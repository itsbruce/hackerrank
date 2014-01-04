object Solution extends App {
  def f[A](xs: List[A]): List[A] = xs.foldLeft(List.empty[A])((ys, y) => y :: ys)
  println(f(io.Source.stdin.getLines.toList.map(_.trim).map(_.toInt)).mkString("\n"))
}
