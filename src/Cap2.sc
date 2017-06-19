import scala.util.Try

val beginning = (0 to 10).toList.map(i => Try(i / 0))

val dip =
  beginning.map {
    i =>
    for {
      j <- i
      z <- Try(j * 6)
      k <- Try(z + 3)
    } yield {
      k + 42
    }
  }

val d = beginning.map(i => i.flatMap(j => Try(j * 6)).flatMap(z => Try(z + 3)).map(k => k + 42))